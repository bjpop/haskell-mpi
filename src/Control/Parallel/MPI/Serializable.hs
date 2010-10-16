{-# LANGUAGE ScopedTypeVariables #-}
module Control.Parallel.MPI.Serializable
   ( send
   , bsend
   , ssend
   , rsend
   , sendBS
   , recv
   , recvBS
   , isend
   , ibsend
   , issend
   , isendBS
   , waitall
   , recvFuture
   , bcast
   , sendGather
   , recvGather
   , sendScatter
   , recvScatter
   , allgather
   , alltoall
   ) where

import C2HS
import Control.Concurrent (forkIO)
import Control.Concurrent.MVar (newEmptyMVar, putMVar)
import Control.Monad (when)
import Data.ByteString.Unsafe as BS
import qualified Data.ByteString as BS
import Data.Serialize (encode, decode, Serialize)
import qualified Control.Parallel.MPI.Storable as Storable
import qualified Control.Parallel.MPI.Internal as Internal
import Control.Parallel.MPI.Datatype as Datatype
import Control.Parallel.MPI.Comm as Comm
import Control.Parallel.MPI.Request as Request
import Control.Parallel.MPI.Status as Status
import Control.Parallel.MPI.Utils (checkError)
import Control.Parallel.MPI.Tag as Tag
import Control.Parallel.MPI.Rank as Rank
import Control.Parallel.MPI.Common
import qualified Data.Array.Storable as SA
import Data.List (unfoldr)

send, bsend, ssend, rsend :: Serialize msg => Comm -> Rank -> Tag -> msg -> IO ()
send  c r t m = sendBSwith Internal.send  c r t $ encode m
bsend c r t m = sendBSwith Internal.bsend c r t $ encode m
ssend c r t m = sendBSwith Internal.ssend c r t $ encode m
rsend c r t m = sendBSwith Internal.rsend c r t $ encode m

sendBS :: Comm -> Rank -> Tag -> BS.ByteString -> IO ()
sendBS = sendBSwith Internal.send

sendBSwith ::
  (Ptr () -> CInt -> Datatype -> CInt -> CInt -> Comm -> IO CInt) ->
  Comm -> Rank -> Tag -> BS.ByteString -> IO ()
sendBSwith send_function comm rank tag bs = do
   let cRank = fromRank rank
       cTag  = fromTag tag
       cCount = cIntConv $ BS.length bs
   unsafeUseAsCString bs $ \cString ->
       checkError $ send_function (castPtr cString) cCount byte cRank cTag comm

recv :: Serialize msg => Comm -> Rank -> Tag -> IO (msg, Status)
recv comm rank tag = do
   (bs, status) <- recvBS comm rank tag
   case decode bs of
      Left e -> fail e
      Right val -> return (val, status)

recvBS :: Comm -> Rank -> Tag -> IO (BS.ByteString, Status)
recvBS comm rank tag = do
   probeStatus <- probe rank tag comm
   let count = fromIntegral $ status_count probeStatus
       cSource = fromRank rank
       cTag    = fromTag tag
       cCount  = cIntConv count
   allocaBytes count
      (\bufferPtr ->
          alloca $ \statusPtr -> do
             checkError $ Internal.recv bufferPtr cCount byte cSource cTag comm $ castPtr statusPtr
             recvStatus <- peek statusPtr
             message <- BS.packCStringLen (castPtr bufferPtr, count)
             return (message, recvStatus))

isend, ibsend, issend :: Serialize msg => Comm -> Rank -> Tag -> msg -> IO Request
isend  c r t m = isendBSwith Internal.isend  c r t $ encode m
ibsend c r t m = isendBSwith Internal.ibsend c r t $ encode m
issend c r t m = isendBSwith Internal.issend c r t $ encode m

isendBS :: Comm -> Rank -> Tag -> BS.ByteString -> IO Request
isendBS = isendBSwith Internal.isend

isendBSwith ::
  (Ptr () -> CInt -> Datatype -> CInt -> CInt -> Comm -> Ptr (Request) -> IO CInt) ->
  Comm -> Rank -> Tag -> BS.ByteString -> IO Request
isendBSwith send_function comm rank tag bs = do
   let cRank = fromRank rank
       cTag  = fromTag tag
       cCount = cIntConv $ BS.length bs
   alloca $ \requestPtr ->
      unsafeUseAsCString bs $ \cString -> do
          checkError $ send_function (castPtr cString) cCount byte cRank cTag comm requestPtr
          peek requestPtr

waitall :: [Request] -> IO [Status]
waitall reqs = do
  withArrayLen reqs $ \len reqPtr ->
    allocaArray len $ \statPtr -> do
      checkError $ Internal.waitall (cIntConv len) reqPtr (castPtr statPtr)
      peekArray len statPtr

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

bcast :: Serialize msg => Comm -> Rank -> msg -> IO msg
bcast comm rootRank msg = do
   myRank <- commRank comm
   -- Intercommunicators are handled differently.
   -- Basically, if communicator is intercommunicator, it means that
   -- there are two groups of processes - sending group and
   -- receiving group. From the sending group only one process
   -- actually sends the data - the one that specifies
   -- "theRoot" as the value of rootRank. All other processes from the
   -- sending group should specify "procNull" as the
   -- rootRank and (if I understand MPI specs properly)
   -- would disregard "sending buffer" argument and would
   -- not actually send anything. That's why for procNull ranks we
   -- use empty ByteString as payload.
   isInter <- commTestInter comm
   if isInter then if rootRank == theRoot then doSend (encode msg)
                   else if rootRank ==  procNull then doSend BS.empty -- do nothing
                        else doRecv
     else -- intra-communicator, i.e. a single homogenous group of processes.
     if (myRank == rootRank)
     then doSend (encode msg)
     else doRecv
  where
    doSend bs = do
      -- broadcast the size of the message first
      Storable.bcastSend comm rootRank (cIntConv (BS.length bs) :: CInt)
      -- then broadcast the actual message
      Storable.bcastSend comm rootRank bs
      return msg
    doRecv = do
      -- receive the broadcast of the size
      (count::CInt) <- Storable.intoNewVal_ $ Storable.bcastRecv comm rootRank
      -- receive the broadcast of the message
      bs <- Storable.intoNewBS_ count $ Storable.bcastRecv comm rootRank
      case decode bs of
        Left e -> fail e
        Right val -> return val

-- List should have exactly numProcs elements
sendGather :: Serialize msg => Comm -> Rank -> msg -> IO ()
sendGather comm root msg = do
  let enc_msg = encode msg
  -- Send length
  Storable.sendGather comm root (cIntConv (BS.length enc_msg) :: CInt)
  -- Send payload
  Storable.sendGatherv comm root enc_msg
  
recvGather :: Serialize msg => Comm -> Rank -> msg -> IO [msg]
recvGather comm root msg = do
  isInter <- commTestInter comm
  if isInter then if root == procNull then return []
                  else if root == theRoot then doRecv isInter
                       else fail "Process in receiving group of intercommunicator uses unsupported value of root in recvGather"
    else doRecv isInter
  where
    doRecv isInter = do
      let enc_msg = encode msg
      numProcs <- if isInter then commRemoteSize comm else commSize comm
      (lengthsArr :: SA.StorableArray Int CInt) <- Storable.intoNewArray_ (0,numProcs-1) $ Storable.recvGather comm root (cIntConv (BS.length enc_msg) :: CInt) 
      -- calculate displacements from sizes
      lengths <- SA.getElems lengthsArr
      (displArr :: SA.StorableArray Int CInt) <- SA.newListArray (0,numProcs-1) $ Prelude.init $ scanl1 (+) (0:lengths)
      bs <- Storable.intoNewBS_ (sum lengths) $ Storable.recvGatherv comm root enc_msg lengthsArr displArr
      return $ decodeList lengths bs

decodeList :: (Serialize msg) => [CInt] -> BS.ByteString -> [msg]
decodeList lengths bs = unfoldr decodeNext (lengths,bs)
  where
    decodeNext ([],_) = Nothing
    decodeNext ((l:ls),bs) = 
      case decode bs of
        Left e -> fail e
        Right val -> Just (val, (ls, BS.drop (cIntConv l) bs))
        
recvScatter :: Serialize msg => Comm -> Rank -> IO msg
recvScatter comm root = do
  -- Recv length
  (len::CInt) <- Storable.intoNewVal_ $ Storable.recvScatter comm root
  -- Recv payload
  bs <- Storable.intoNewBS_ len $ Storable.recvScatterv comm root
  case decode bs of
    Left e -> fail e
    Right val -> return val
    
-- XXX: List should have exactly numProcs elements  
sendScatter :: Serialize msg => Comm -> Rank -> [msg] -> IO msg
sendScatter comm root msgs = do
  isInter <- commTestInter comm
  numProcs <- if isInter then commRemoteSize comm else commSize comm
  when (length msgs /= numProcs) $ fail "Unable to deliver one message to each receiving process in sendScatter"
  if isInter then if root == procNull then return $ head msgs 
                                           -- XXX:
                                           -- fix this. We really 
                                           -- should just return ()
                                           -- here.
                  else if root == theRoot then doSend
                       else fail "Process in sending group of intercommunicator uses unsupported value of root in sendScatter"
    else doSend -- intracommunicator
  where
    doSend = do
      let enc_msgs = map encode msgs
          lengths = map (cIntConv . BS.length) enc_msgs
          payload = BS.concat enc_msgs
          numProcs = length msgs
      -- scatter numProcs ints - sizes of payloads to be sent to other processes
      (lengthsArr :: SA.StorableArray Int CInt) <- SA.newListArray (0,numProcs-1) lengths
      (myLen :: CInt) <- Storable.intoNewVal_ $ Storable.sendScatter comm root lengthsArr
      -- calculate displacements from sizes
      (displArr :: SA.StorableArray Int CInt) <- SA.newListArray (0,numProcs-1) $ Prelude.init $ scanl1 (+) (0:lengths)
      -- scatter payloads
      bs <- Storable.intoNewBS_ myLen $ Storable.sendScatterv comm root payload lengthsArr displArr
      case decode bs of
        Left e -> fail e
        Right val -> return val

allgather :: (Serialize msg) => Comm -> msg -> IO [msg]
allgather comm msg = do
  let enc_msg = encode msg
  isInter <- commTestInter comm
  numProcs <- if isInter then commRemoteSize comm else commSize comm      
  -- Send length of my message and receive lengths from other ranks
  (lengthsArr :: SA.StorableArray Int CInt) <- Storable.intoNewArray_ (0, numProcs-1) $ Storable.allgather comm (cIntConv (BS.length enc_msg) :: CInt)
  -- calculate displacements from sizes
  lengths <- SA.getElems lengthsArr
  (displArr :: SA.StorableArray Int CInt) <- SA.newListArray (0,numProcs-1) $ Prelude.init $ scanl1 (+) (0:lengths)
  -- Send my payload and receive payloads from other ranks
  bs <- Storable.intoNewBS_ (sum lengths) $ Storable.allgatherv comm enc_msg lengthsArr displArr
  return $ decodeList lengths bs

alltoall :: (Serialize msg) => Comm -> [msg] -> IO [msg]
alltoall comm msgs = do
  let enc_msgs = map encode msgs
      sendLengths = map (cIntConv . BS.length) enc_msgs
      sendPayload = BS.concat enc_msgs
  isInter <- commTestInter comm
  numProcs <- if isInter then commRemoteSize comm else commSize comm      
  -- First, all-to-all payload sizes
  (sendLengthsArr :: SA.StorableArray Int CInt) <- SA.newListArray (1,numProcs) sendLengths
  (recvLengthsArr :: SA.StorableArray Int CInt) <- Storable.intoNewArray_ (1,numProcs) $ Storable.alltoall comm sendLengthsArr 1
  recvLengths <- SA.getElems recvLengthsArr
  -- calculate displacements from sizes
  (sendDisplArr :: SA.StorableArray Int CInt) <- SA.newListArray (1,numProcs) $ Prelude.init $ scanl1 (+) (0:sendLengths)
  (recvDisplArr :: SA.StorableArray Int CInt) <- SA.newListArray (1,numProcs) $ Prelude.init $ scanl1 (+) (0:recvLengths)
  -- Receive payloads
  bs <- Storable.intoNewBS_ (sum recvLengths) $ Storable.alltoallv comm sendPayload sendLengthsArr sendDisplArr recvLengthsArr recvDisplArr
  return $ decodeList recvLengths bs
  
