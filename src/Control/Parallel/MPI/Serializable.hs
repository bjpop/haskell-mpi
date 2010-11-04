{-# LANGUAGE ScopedTypeVariables #-}

-----------------------------------------------------------------------------
-- |
-- Module      : Control.Parallel.MPI.Serializable
-- Copyright   : (c) 2010 Bernie Pope, Dmitry Astapov
-- License     : BSD-style
-- Maintainer  : florbitous@gmail.com
-- Stability   : experimental
-- Portability : ghc
--
-- This module provides MPI functionality for arbitrary Haskell values that are
-- instances of Storable typeclass.
--
-- Since low-level MPI calls have to know the size of transmitted message, all
-- functions in this module internally make one extra call to transfer the size
-- of encoded message to receiving side prior to transmitting the message itself.
-- Obviously, this incurs some overhead.
--
-- Full range of point-to-point and collective operation is supported, except for reduce and similar operations.
-- Low-level MPI reduction operations could not be used on values whose structure is hidden from MPI (which is
-- exactly the case here), and implementation of reduction in Haskell heavily depends on the nature of data being
-- processed, so there is no need to try and implement some general case in this module.
--
-- Below is a small but complete MPI program utilising this Module. Process 1 sends the message
-- @\"Hello World\"@ to process 0. Process 0 receives the message and prints it
-- to standard output. It assumes that there are at least 2 MPI processes
-- available. Further examples in this module would provide different implementation of
-- @process@ function.
--
-- >module Main where
-- >
-- >import Control.Parallel.MPI (mpi, commRank, commWorld, unitTag)
-- >import Control.Parallel.MPI.Serializable (send, recv)
-- >
-- >main :: IO ()
-- >main = mpi $ do
-- >   rank <- commRank commWorld
-- >   process rank
-- >
-- >process rank
-- >   | rank == 1 = send commWorld 0 unitTag "Hello World"
-- >   | rank == 0 = do
-- >      (msg, _status) <- recv commWorld 1 unitTag
-- >      putStrLn msg
-- >   | otherwise = return () -- do nothing
-----------------------------------------------------------------------------

module Control.Parallel.MPI.Serializable
   (
     -- * Point-to-point communication functions
     -- ** Blocking
     send
   , bsend
   , ssend
   , rsend
   , recv
     -- ** Non-blocking
   , isend
   , ibsend
   , issend
   , recvFuture
   , waitall

     -- ** Low-level (operating on ByteStrings)
   , sendBS
   , recvBS
   , isendBS
     -- | Here is how you can use those functions
     --
     -- @
     -- process rank
     --   | rank == 0 = do sendBS 'commWorld' 1 123 (BS.Pack \"Hello world!\")
     --                    request <- isendBS 'commWorld' 2 123 (BS.Pack \"And you too!\")
     --                    'wait' request
     --   | rank \`elem\` [1,2] = do (msg, status) <- recvBS 'commWorld' 0 123
     --                            print msg
     --   | otherwise = return ()
     -- @

     -- * Collective operations
     {- | Broadcast and other collective operations are tricky because the receiver doesn't know how much memory to allocate.
     The C interface assumes the sender and receiver agree on the size in advance, but
     this is not useful for the Haskell interface (where we want to send arbitrary sized
     values) because the sender is the only process which has the actual data available.

     The work around is for the sender to send two messages. The first says how much data
     is coming. The second message sends the actual data. We rely on the two messages being
     sent and received in this order. Conversely the receiver gets two messages. The first is
     the size of memory to allocate and the second in the actual message.

     The obvious downside of this approach is that it requires two MPI calls for one
     payload.
     -}
     -- ** One-to-all
   , bcastSend
   , bcastRecv
   , scatterSend
   , scatterRecv
     -- ** All-to-one
   , gatherSend
   , gatherRecv
   , allgather
     -- ** All-to-all
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
import Control.Parallel.MPI
import qualified Data.Array.Storable as SA
import Data.List (unfoldr)

-- | Serializes the supplied value to ByteString and sends to specified process (using @MPI_Send@)
--
--  This call could complete before the matching receive is posted by some other process.
send :: Serialize msg => Comm -> Rank -> Tag -> msg -> IO ()
send  c r t m = sendBSwith Internal.send  c r t $ encode m

-- | Serializes the supplied value and sends to specified process (using @MPI_BSend@)
--
--   Application has to allocate the buffer and @attach@ it to MPI using (TODO: we are currently missing this)
bsend :: Serialize msg => Comm -> Rank -> Tag -> msg -> IO ()
bsend c r t m = sendBSwith Internal.bsend c r t $ encode m

-- | Serializes the supplied value and sends to specified process (using @MPI_SSend@)
--
--   This is so-called \"synchronous blocking send\" mode - this call would not complete until
--   matching receive is posted and started to receive data.
ssend :: Serialize msg => Comm -> Rank -> Tag -> msg -> IO ()
ssend c r t m = sendBSwith Internal.ssend c r t $ encode m

-- | Serializes the supplied value and sends to specified process (using @MPI_RSend@)
--
--   This call expects the matching receive already to be posted, otherwise error will occur.
rsend :: Serialize msg => Comm -> Rank -> Tag -> msg -> IO ()
rsend c r t m = sendBSwith Internal.rsend c r t $ encode m

-- | Sends ByteString to specified process. Internally uses @MPI_Send@.
sendBS :: Comm -> Rank -> Tag -> BS.ByteString -> IO ()
sendBS = sendBSwith Internal.send

sendBSwith ::
  (Ptr () -> CInt -> Datatype -> Rank -> Tag -> Comm -> IO ()) ->
  Comm -> Rank -> Tag -> BS.ByteString -> IO ()
sendBSwith send_function comm rank tag bs = do
   let cCount = cIntConv $ BS.length bs
   unsafeUseAsCString bs $ \cString ->
       send_function (castPtr cString) cCount byte rank tag comm

-- | Receives arbitrary serializable message from specified process. Operation status
-- is returned as second component of the tuple, and usually could be discarded.
--
-- This function uses @MPI_Recv@ internally
recv :: Serialize msg => Comm -> Rank -> Tag -> IO (msg, Status)
recv comm rank tag = do
   (bs, status) <- recvBS comm rank tag
   case decode bs of
      Left e -> fail e
      Right val -> return (val, status)

-- | Receives ByteString from specified process. Internally uses @MPI_Recv@.
recvBS :: Comm -> Rank -> Tag -> IO (BS.ByteString, Status)
recvBS comm rank tag = do
   probeStatus <- probe rank tag comm
   let count = fromIntegral $ status_count probeStatus
       cCount  = cIntConv count
   allocaBytes count
      (\bufferPtr -> do
          recvStatus <- Internal.recv bufferPtr cCount byte rank tag comm
          message <- BS.packCStringLen (castPtr bufferPtr, count)
          return (message, recvStatus))

-- | Sends message to specified process in non-blocking mode (using @MPI_ISend@).
--
-- User have to utilise `wait' on the
-- returned `Request' object to find out when operation is completed.
-- In this case it actually means \"data has been copied to the internal MPI buffer\" - no
-- check for matching `recv' being posted is done.
--
-- Example:
--
-- @
-- do req <- isend 'commWorld' 0 'unitTag' \"Hello world!\"
--    'wait' req
-- @
isend  :: Serialize msg => Comm -> Rank -> Tag -> msg -> IO Request
isend  c r t m = isendBSwith Internal.isend  c r t $ encode m

-- | Sends message to specified process utilising buffer attached with TODO in non-blocking mode (using @MPI_BSend@)
ibsend :: Serialize msg => Comm -> Rank -> Tag -> msg -> IO Request
ibsend c r t m = isendBSwith Internal.ibsend c r t $ encode m

-- | Sends message to specified process in non-blocking mode (using @MPI_BSend@)
--
-- Calling `wait' on returned `Request' object would complete once the receiving
-- process has actually started receiving data.
issend :: Serialize msg => Comm -> Rank -> Tag -> msg -> IO Request
issend c r t m = isendBSwith Internal.issend c r t $ encode m

-- | Sends ByteString to specified process in non-blocking mode (using @MPI_ISend@).
isendBS :: Comm -> Rank -> Tag -> BS.ByteString -> IO Request
isendBS = isendBSwith Internal.isend

isendBSwith ::
  (Ptr () -> CInt -> Datatype -> Rank -> Tag -> Comm -> IO Request) ->
  Comm -> Rank -> Tag -> BS.ByteString -> IO Request
isendBSwith send_function comm rank tag bs = do
   let cCount = cIntConv $ BS.length bs
   unsafeUseAsCString bs $ \cString -> do
       send_function (castPtr cString) cCount byte rank tag comm

-- | Blocking test for completion of all specified `Request' objects
--
-- Example. Posting 100 sends and waiting until all of them complete:
--
-- >do requests <- forM ([0..99]) $ \s ->
-- >     isend commWorld someRank unitTag (take s longMessage)
-- >   waitall requests
waitall :: [Request] -> IO [Status]
waitall reqs = do
  withArrayLen reqs $ \len reqPtr ->
    allocaArray len $ \statPtr -> do
      Internal.waitall (cIntConv len) reqPtr (castPtr statPtr)
      peekArray len statPtr

-- | Non-blocking receive of the message. Returns value of type `Future',
-- which could be used to check status of the operation using `getFutureStatus'
-- and extract actual value using either `waitFuture' or `pollFuture':
--
-- Example:
--
-- >do f <- recvFuture commWorld someRank unitTag
-- >   value <- waitFuture f
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

-- | Broadcasts message to all members of specified inter- or intra-communicator.
-- `Rank' of the sending process should be provided, as mandated by MPI.
--
-- This function handles both inter- and intracommunicators, provided that the caller makes proper use of `theRoot' and `procNull'
--
-- See `bcastRecv' for complete example.
bcastSend :: Serialize msg => Comm -> Rank -> msg -> IO ()
bcastSend comm rootRank msg = do
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
                        else fail "bcastSend with intercommunicator accepts either theRoot or procNull as Rank"
     else -- intra-communicator, i.e. a single homogenous group of processes.
     doSend (encode msg)
  where
    doSend bs = do
      -- broadcast the size of the message first
      Storable.bcastSend comm rootRank (cIntConv (BS.length bs) :: CInt)
      -- then broadcast the actual message
      Storable.bcastSend comm rootRank bs

{- | Receive the message being broadcasted in the communicator from the process with specified `Rank'

Example:

>process rank
>  | rank == 0 = bcastSend commWorld 0 "Hello world!"
>  | otherwise = bcastRecv commWorld 0 >>= print
-}
bcastRecv :: Serialize msg => Comm -> Rank -> IO msg
bcastRecv comm rootRank = do
  -- receive the broadcast of the size
  (count::CInt) <- Storable.intoNewVal_ $ Storable.bcastRecv comm rootRank
  -- receive the broadcast of the message
  bs <- Storable.intoNewBS_ count $ Storable.bcastRecv comm rootRank
  case decode bs of
    Left e -> fail e
    Right val -> return val

-- | Send a message to the specified process, to be collected using `gatherRecv'.
gatherSend :: Serialize msg => Comm -> Rank -> msg -> IO ()
gatherSend comm root msg = do
  let enc_msg = encode msg
  -- Send length
  Storable.gatherSend comm root (cIntConv (BS.length enc_msg) :: CInt)
  -- Send payload
  Storable.gathervSend comm root enc_msg

{- | Collects the messages sent with `gatherSend' and returns them as list.
Note that per MPI semantics collecting process is expected to supply the message as well.

This function handles both inter- and intracommunicators, provided that the caller makes proper use of `theRoot' and `procNull'.

Example. Gathering rank numbers from all processes to the process with rank 0:

>process rank
>  | rank == 0 = do ranks <- gatherRecv commWorld 0 rank
>                   putStrLn $ "Got messages from ranks:" ++ show ranks
>  | otherwise = gatherSend commWorld 0 rank
-}
gatherRecv :: Serialize msg => Comm -> Rank -> msg -> IO [msg]
gatherRecv comm root msg = do
  isInter <- commTestInter comm
  if isInter then if root == procNull then return []
                  else if root == theRoot then doRecv isInter
                       else fail "Process in receiving group of intercommunicator uses unsupported value of root in gatherRecv"
    else doRecv isInter
  where
    doRecv isInter = do
      let enc_msg = encode msg
      numProcs <- if isInter then commRemoteSize comm else commSize comm
      (lengthsArr :: SA.StorableArray Int CInt) <- Storable.intoNewArray_ (0,numProcs-1) $ Storable.gatherRecv comm root (cIntConv (BS.length enc_msg) :: CInt)
      -- calculate displacements from sizes
      lengths <- SA.getElems lengthsArr
      (displArr :: SA.StorableArray Int CInt) <- SA.newListArray (0,numProcs-1) $ Prelude.init $ scanl1 (+) (0:lengths)
      bs <- Storable.intoNewBS_ (sum lengths) $ Storable.gathervRecv comm root enc_msg lengthsArr displArr
      return $ decodeList lengths bs

decodeList :: (Serialize msg) => [CInt] -> BS.ByteString -> [msg]
decodeList lengths bs = unfoldr decodeNext (lengths,bs)
  where
    decodeNext ([],_) = Nothing
    decodeNext ((l:ls),bs) =
      case decode bs of
        Left e -> fail e
        Right val -> Just (val, (ls, BS.drop (cIntConv l) bs))

{- | Receives single message from the process that distributes them with `scatterSend'

Example. Scattering @\"Hello world\"@ to all processes from process with rank 0:

>process rank
>   | rank == 0 = do n <- commSize commWorld
>                    myMsg <- scatterSend commWorld 0 $ replicate n "Hello World!"
>   | otherwise = do msg <- scatterRecv commWorld 0
>                    print msg
-}
scatterRecv :: Serialize msg => Comm -> Rank -> IO msg
scatterRecv comm root = do
  -- Recv length
  (len::CInt) <- Storable.intoNewVal_ $ Storable.scatterRecv comm root
  -- Recv payload
  bs <- Storable.intoNewBS_ len $ Storable.scattervRecv comm root
  case decode bs of
    Left e -> fail e
    Right val -> return val

-- | Distributes a list of messages between processes in the given communicator
-- so that each process gets exactly one message. It is caller's responsibility
-- to ensure that list has proper amount of messages (error would be raised otherwise).
--
-- This function handles both inter- and intracommunicators.
scatterSend :: Serialize msg => Comm -> Rank -> [msg] -> IO msg
scatterSend comm root msgs = do
  isInter <- commTestInter comm
  numProcs <- if isInter then commRemoteSize comm else commSize comm
  when (length msgs /= numProcs) $ fail "Unable to deliver one message to each receiving process in scatterSend"
  if isInter then if root == procNull then return $ head msgs
                                           -- XXX:
                                           -- fix this. We really
                                           -- should just return ()
                                           -- here.
                  else if root == theRoot then doSend
                       else fail "Process in sending group of intercommunicator uses unsupported value of root in scatterSend"
    else doSend -- intracommunicator
  where
    doSend = do
      let enc_msgs = map encode msgs
          lengths = map (cIntConv . BS.length) enc_msgs
          payload = BS.concat enc_msgs
          numProcs = length msgs
      -- scatter numProcs ints - sizes of payloads to be sent to other processes
      (lengthsArr :: SA.StorableArray Int CInt) <- SA.newListArray (0,numProcs-1) lengths
      (myLen :: CInt) <- Storable.intoNewVal_ $ Storable.scatterSend comm root lengthsArr
      -- calculate displacements from sizes
      (displArr :: SA.StorableArray Int CInt) <- SA.newListArray (0,numProcs-1) $ Prelude.init $ scanl1 (+) (0:lengths)
      -- scatter payloads
      bs <- Storable.intoNewBS_ myLen $ Storable.scattervSend comm root payload lengthsArr displArr
      case decode bs of
        Left e -> fail e
        Right val -> return val

{- | All processes in the given communicator supply a message. This list of messages is then received
by every process in the communicator. Value returned from this function would be identical across
all processes.

This function handles both inter- and intracommunicators.

Example. Each process shares it's rank number, so that all processes have to full list of all participating ranks:

> process rank = do ranks <- allgather commWorld rank
>                   putStrLn $ "Participating ranks:" ++ show ranks
-}
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

{- | Each processes in the given communicator sends one message to every other process
and receives a list of messages, one from every process in the communicator.

This function handles both inter- and intracommunicators.

Example. Each process sends his own rank (as a list @[rank]@) to process with rank 0, @[rank, rank]@ to process with rank 1, and so on.
Therefore, process with rank 0 gets @[[0],[1],[2]]@, process with rank 1 gets @[[0,0],[1,1],[2,2]]@ and so on:

> process rank = do
>  numProcs <- commSize commWorld
>  let msg = take numProcs $ map (`take` (repeat rank)) [1..]
>  result <- alltoall commWorld msg
>  putStrLn $ "Rank " ++ show rank ++ " got message " ++ show result
-}
alltoall :: (Serialize msg) => Comm -> [msg] -> IO [msg]
alltoall comm msgs = do
  let enc_msgs = map encode msgs
      sendLengths = map (cIntConv . BS.length) enc_msgs
      sendPayload = BS.concat enc_msgs
  isInter <- commTestInter comm
  numProcs <- if isInter then commRemoteSize comm else commSize comm
  when (length msgs /= numProcs) $ fail "Unable to deliver one message to each receiving process in alltoall"
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

