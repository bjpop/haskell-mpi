{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Control.Parallel.MPI.Serializable
   ( module Datatype
   , module Comm
   , module Status
   , module Tag
   , module Rank
   , mpi
   , init
   , finalize
   , commSize
   , commRank
   , probe
   , send
   , sendBS
   , recv
   , recvBS
   , iSend
   , iSendBS
   , Future
   , cancelFuture
   , pollFuture
   , waitFuture
   , getFutureStatus
   , recvFuture
   , bcast
   , barrier
   , wait
   ) where

import Prelude hiding (init)
import C2HS
import Control.Concurrent (forkOS, forkIO, ThreadId, killThread)
import Control.Concurrent.MVar (MVar, newEmptyMVar, tryTakeMVar, readMVar, putMVar)
import Data.ByteString.Unsafe as BS
import qualified Data.ByteString as BS
import Data.Serialize (encode, decode, Serialize)
import qualified Control.Parallel.MPI.Internal as Internal 
import Control.Parallel.MPI.Datatype as Datatype
import Control.Parallel.MPI.Comm as Comm
import Control.Parallel.MPI.Request as Request
import Control.Parallel.MPI.Status as Status
import Control.Parallel.MPI.Utils (checkError)
import Control.Parallel.MPI.Tag as Tag
import Control.Parallel.MPI.Rank as Rank

mpi :: IO () -> IO ()
mpi action = init >> action >> finalize

init :: IO ()
init = checkError Internal.init

finalize :: IO ()
finalize = checkError Internal.finalize

commSize :: Comm -> IO Int
commSize comm = do
   alloca $ \ptr -> do
      checkError $ Internal.commSize comm ptr
      size <- peek ptr
      return $ cIntConv size

commRank :: Comm -> IO Rank
commRank comm =
   alloca $ \ptr -> do
      checkError $ Internal.commRank comm ptr
      rank <- peek ptr
      return $ toRank rank

probe :: Rank -> Tag -> Comm -> IO Status
probe rank tag comm = do
   let cSource = fromRank rank
       cTag    = fromTag tag
   alloca $ \statusPtr -> do
      checkError $ Internal.probe cSource cTag comm $ castPtr statusPtr
      peek statusPtr

send :: Serialize msg => msg -> Rank -> Tag -> Comm -> IO ()
send = sendBS . encode

sendBS :: BS.ByteString -> Rank -> Tag -> Comm -> IO ()
sendBS bs rank tag comm = do
   let cRank = fromRank rank
       cTag  = fromTag tag
       cCount = cIntConv $ BS.length bs
   unsafeUseAsCString bs $ \cString ->
       checkError $ Internal.send (castPtr cString) cCount byte cRank cTag comm

recv :: Serialize msg => Rank -> Tag -> Comm -> IO (Status, msg)
recv rank tag comm = do
   (status, bs) <- recvBS rank tag comm
   case decode bs of
      Left e -> fail e
      Right val -> return (status, val)

recvBS :: Rank -> Tag -> Comm -> IO (Status, BS.ByteString)
recvBS rank tag comm = do
   probeStatus <- probe rank tag comm
   let count = status_count probeStatus
       cSource = fromRank rank
       cTag    = fromTag tag
       cCount  = cIntConv count
   allocaBytes count
      (\bufferPtr ->
          alloca $ \statusPtr -> do
             checkError $ Internal.recv bufferPtr cCount byte cSource cTag comm $ castPtr statusPtr
             recvStatus <- peek statusPtr
             message <- BS.packCStringLen (castPtr bufferPtr, count)
             return (recvStatus, message))

iSend :: Serialize msg => msg -> Rank -> Tag -> Comm -> IO Request
iSend = iSendBS . encode

iSendBS :: BS.ByteString -> Rank -> Tag -> Comm -> IO Request
iSendBS bs rank tag comm = do
   let cRank = fromRank rank
       cTag  = fromTag tag
       cCount = cIntConv $ BS.length bs
   alloca $ \requestPtr ->
      unsafeUseAsCString bs $ \cString -> do
          checkError $ Internal.iSend (castPtr cString) cCount byte cRank cTag comm requestPtr
          peek requestPtr

data Future a =
   Future
   { futureThread :: ThreadId
   , futureStatus :: MVar Status
   , futureVal :: MVar a
   }

waitFuture :: Future a -> IO a
waitFuture = readMVar . futureVal

getFutureStatus :: Future a -> IO Status
getFutureStatus = readMVar . futureStatus

pollFuture :: Future a -> IO (Maybe a)
pollFuture = tryTakeMVar . futureVal

-- May want to stop people from waiting on Futures which are killed...
cancelFuture :: Future a -> IO ()
cancelFuture = killThread . futureThread

recvFuture :: Serialize msg => Rank -> Tag -> Comm -> IO (Future msg)
recvFuture rank tag comm = do
   valRef <- newEmptyMVar
   statusRef <- newEmptyMVar
   -- is forkIO acceptable here? Depends on thread local stateness of MPI.
   -- threadId <- forkOS $ do
   threadId <- forkIO $ do
      -- do a synchronous recv in another thread
      (status, msg) <- recv rank tag comm
      putMVar valRef msg
      putMVar statusRef status
   return $ Future { futureThread = threadId, futureStatus = statusRef, futureVal = valRef }

{- Broadcast is tricky because the receiver doesn't know how much memory to allocate.
   The C interface assumes the sender and receiver agree on the size in advance, but
   this is not useful for the Haskell interface (where we want to send arbitrary sized
   values) because the sender is the only process which has the actual data available

   The work around is for the sender to send two messages. The first says how much data
   is coming. The second message sends the actual data. We rely on the two messages being
   sent and received in this order. Conversely the receiver gets two messages. The first is
   the size of memory to allocate and the second in the actual message.

   The obvious downside of this approach is that it requires two broadcasts for one
   payload. Communication costs can be expensive.

   The idea for this scheme was inspired by the Ocaml bindings. Therefore there is
   some precedent for doing it this way.
-}

bcast :: Serialize msg => msg -> Rank -> Comm -> IO msg
bcast msg rootRank comm = do
   myRank <- commRank comm
   let cRank  = fromRank rootRank
   if myRank == rootRank
      then do
         let bs = encode msg
             cCount = cIntConv $ BS.length bs
         -- broadcast the size of the message first
         alloca $ \ptr -> do
            poke ptr cCount
            let numberOfInts = 1::CInt
            checkError $ Internal.bcast (castPtr ptr) numberOfInts int cRank comm
         -- then broadcast the actual message
         unsafeUseAsCString bs $ \cString -> do
            checkError $ Internal.bcast (castPtr cString) cCount byte cRank comm
         return msg
      else do
         -- receive the broadcast of the size
         count <- alloca $ \ptr -> do
            checkError $ Internal.bcast (castPtr ptr) 1 int cRank comm
            peek ptr
         -- receive the broadcast of the message
         allocaBytes count $
            \bufferPtr -> do
               let cCount = cIntConv count
               checkError $ Internal.bcast bufferPtr cCount byte cRank comm 
               bs <- BS.packCStringLen (castPtr bufferPtr, count)  
               case decode bs of
                  Left e -> fail e
                  Right val -> return val

barrier :: Comm -> IO ()
barrier comm = checkError $ Internal.barrier comm

wait :: Request -> IO Status
wait request =
   alloca $ \statusPtr ->
     alloca $ \reqPtr -> do
       s <- peek statusPtr
       poke reqPtr request
       checkError $ Internal.wait reqPtr (castPtr statusPtr)
       peek statusPtr
