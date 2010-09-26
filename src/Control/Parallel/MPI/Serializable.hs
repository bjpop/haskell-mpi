{-# LANGUAGE ScopedTypeVariables #-}

module Control.Parallel.MPI.Serializable
   ( send
   , bsend
   , ssend
   , rsend
   , recv
   , isend
   , ibsend
   , issend
   , recvFuture
   , bcastSend
   , bcastRecv
   ) where

import Control.Concurrent.MVar (newEmptyMVar, putMVar)
import Control.Concurrent (forkIO)
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS (length)
import Data.Serialize (encode, decode, Serialize)
import Control.Parallel.MPI.Common
import qualified Control.Parallel.MPI.Messaging as Msg
import Control.Parallel.MPI.Future

sendSer :: Serialize msg => (Comm -> Rank -> Tag -> ByteString -> IO a) -> Comm -> Rank -> Tag -> msg -> IO a
sendSer f comm rank tag msg = f comm rank tag (encode msg :: ByteString)

send, bsend, ssend, rsend :: Serialize msg => Comm -> Rank -> Tag -> msg -> IO ()
send  = sendSer Msg.send
bsend = sendSer Msg.bsend
ssend = sendSer Msg.ssend
rsend = sendSer Msg.rsend

isend, ibsend, issend :: Serialize msg => Comm -> Rank -> Tag -> msg -> IO Request
isend  = sendSer Msg.isend
ibsend = sendSer Msg.ibsend
issend = sendSer Msg.issend

recv :: Serialize msg => Comm -> Rank -> Tag -> IO (msg, Status)
recv comm rank tag = do
   probeStatus <- probe rank tag comm
   let size = status_count probeStatus
   (bs, status) <- Msg.recvImmut comm rank tag size
   case decode bs of
      Left e -> fail e
      Right val -> return (val, status)

recvFuture :: Serialize msg => Comm -> Rank -> Tag -> IO (Future msg)
recvFuture comm rank tag = do
   valRef <- newEmptyMVar
   statusRef <- newEmptyMVar
   -- is forkIO acceptable here? Depends on thread local stateness of MPI.
   -- threadId <- forkOS $ do
   threadId <- forkIO $ do
      -- do a synchronous recv in another thread
      (msg, status) <- recv comm rank tag
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

bcastSend :: Serialize msg => Comm -> Rank -> msg -> IO ()
bcastSend comm rank msg = do
   let bs = encode msg
   -- broadcast the size of the message first
   Msg.bcastSend comm rank $ BS.length bs
   -- then broadcast the actual message
   Msg.bcastSend comm rank bs

bcastRecv :: Serialize msg => Comm -> Rank -> IO msg
bcastRecv comm rank = do
   -- receive the broadcast of the size
   (count :: Int) <- Msg.bcastRecvImmut comm rank ()
   -- receive the broadcast of the message
   bs <- Msg.bcastRecvImmut comm rank count
   case decode bs of
      Left e -> fail e
      Right val -> return val
