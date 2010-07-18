{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Bindings.MPI 
   ( module Datatype 
   , module Comm
   , module Status
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
   , recvFuture
   , Rank
   , toRank
   , fromRank 
   , rankId
   ) where

import Prelude hiding (init)
import C2HS
import Control.Concurrent (forkOS, forkIO, ThreadId, killThread)
import Control.Concurrent.MVar (MVar, newEmptyMVar, tryTakeMVar, readMVar, putMVar)
import Data.ByteString.Unsafe as BS
import qualified Data.ByteString as BS
import Data.Serialize (encode, decode, Serialize)
import qualified Bindings.MPI.Internal as Internal 
import Bindings.MPI.Datatype as Datatype
import Bindings.MPI.Comm as Comm
import Bindings.MPI.Request as Request
import Bindings.MPI.Status as Status 
import Bindings.MPI.MarshalUtils (enumToCInt)
import Bindings.MPI.Utils (checkError)

mpi :: IO () -> IO ()
mpi action = init >> action >> finalize

newtype Rank = Rank { rankId :: Int }
   deriving (Eq, Ord, Enum)

instance Show Rank where
   show = show . rankId

toRank :: Enum a => a -> Rank
toRank x = Rank { rankId = fromEnum x } 

fromRank :: Enum a => Rank -> a
fromRank = toEnum . rankId 

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

probe :: (Enum tag) => Rank -> tag -> Comm -> IO Status
probe rank tag comm = do
   let cSource = fromRank rank 
       cTag    = enumToCInt tag
   alloca $ \statusPtr -> do
      checkError $ Internal.probe cSource cTag comm (castPtr statusPtr)
      peek statusPtr

send :: (Serialize msg, Enum tag) => msg -> Rank -> tag -> Comm -> IO () 
send = sendBS . encode

sendBS :: (Enum tag) => BS.ByteString -> Rank -> tag -> Comm -> IO () 
sendBS bs rank tag comm = do
   let cRank = fromRank rank 
       cTag  = enumToCInt tag
       cCount = cIntConv $ BS.length bs
   unsafeUseAsCString bs $ \cString -> 
       checkError $ Internal.send (castPtr cString) cCount byte cRank cTag comm

recv :: (Serialize msg, Enum tag) => Rank -> tag -> Comm -> IO (Status, msg)
recv rank tag comm = do
   (status, bs) <- recvBS rank tag comm
   case decode bs of
      Left e -> fail e
      Right val -> return (status, val)
        
recvBS :: (Enum tag) => Rank -> tag -> Comm -> IO (Status, BS.ByteString)
recvBS rank tag comm = do
   probeStatus <- probe rank tag comm
   let count = status_count probeStatus 
       cSource = fromRank rank 
       cTag    = enumToCInt tag
       cCount  = cIntConv count
   allocaBytes count 
      (\bufferPtr -> 
          alloca $ \statusPtr -> do
             checkError $ Internal.recv bufferPtr cCount byte cSource cTag comm $ castPtr statusPtr
             recvStatus <- peek statusPtr
             message <- BS.packCStringLen (castPtr bufferPtr, count)  
             return (recvStatus, message))

iSend :: (Serialize msg, Enum tag) => msg -> Rank -> tag -> Comm -> IO Request 
iSend = iSendBS . encode

iSendBS :: (Enum tag) => BS.ByteString -> Rank -> tag -> Comm -> IO Request 
iSendBS bs rank tag comm = do
   let cRank = fromRank rank 
       cTag  = enumToCInt tag
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

pollFuture :: Future a -> IO (Maybe a)
pollFuture = tryTakeMVar . futureVal 

-- May want to stop people from waiting on Futures which are killed...
cancelFuture :: Future a -> IO ()
cancelFuture = killThread . futureThread

recvFuture :: (Serialize msg, Enum tag) => Rank -> tag -> Comm -> IO (Future msg) 
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
