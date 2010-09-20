{-# LANGUAGE FlexibleContexts, FlexibleInstances, ScopedTypeVariables, MultiParamTypeClasses #-}

module Control.Parallel.MPI.Array
   ( send
   , ssend
   , bsend
   , rsend
   , recv
   , bcastSend
   , bcastRecv
   , isend
   , ibsend
   , issend
   , irecv
   , sendScatter
   , recvScatter
   , sendScatterv
   , recvScatterv
   , sendGather
   , recvGather
   , sendGatherv
   , recvGatherv
   , allgather
   , allgatherv
   , alltoall
   , withNewArray
   , withNewArray_
   ) where

import C2HS
import Data.Word
import Data.Array.Base (unsafeNewArray_)
import Data.Array.IO
import Data.Array.Storable
import Control.Applicative ((<$>))
import qualified Control.Parallel.MPI.Internal as Internal
import Control.Parallel.MPI.Datatype as Datatype
import Control.Parallel.MPI.Comm as Comm
import Control.Parallel.MPI.Status as Status
import Control.Parallel.MPI.Utils (checkError)
import Control.Parallel.MPI.Tag as Tag
import Control.Parallel.MPI.Rank as Rank
import Control.Parallel.MPI.Request as Request

-- | if the user wants to call recvScatterv for the first time without
-- already having allocated the array, then they can call it like so:
--
-- (array,_) <- withNewArray range $ recvScatterv root comm
--
-- and thereafter they can call it like so:
--
--  recvScatterv root comm array
withNewArray :: (Ix i, MArray a e m) => (i, i) -> (a i e -> m r) -> m (a i e, r)
withNewArray range f = do
  arr <- unsafeNewArray_ range -- New, uninitialized array, According to http://hackage.haskell.org/trac/ghc/ticket/3586
                               -- should be faster than newArray_
  res <- f arr
  return (arr, res)

-- | Same as withRange, but discards the result of the processor function
withNewArray_ :: (Ix i, MArray a e m) => (i, i) -> (a i e -> m r) -> m (a i e)
withNewArray_ range f = do
  arr <- unsafeNewArray_ range
  _ <- f arr
  return arr

send, bsend, ssend, rsend :: (Storable e, Ix i, MessageArray a i e) => Comm -> Rank -> Tag -> a i e -> IO ()
send  = sendWith Internal.send
bsend = sendWith Internal.bsend
ssend = sendWith Internal.ssend
rsend = sendWith Internal.rsend

type SendPrim = Ptr () -> CInt -> Datatype -> CInt -> CInt -> Comm -> IO CInt

sendWith :: (Storable e, Ix i, MessageArray a i e) => SendPrim -> Comm -> Rank -> Tag -> a i e -> IO ()
sendWith send_function comm rank tag array = do
   sendFrom array $ \arrayPtr numBytes -> do
      checkError $ send_function (castPtr arrayPtr) numBytes byte (fromRank rank) (fromTag tag) comm

recv :: (Storable e, Ix i, MessageArray a i e) => Comm -> Rank -> Tag -> a i e -> IO Status
recv comm rank tag arr = do
   recvInto arr $ \arrayPtr numBytes ->
      alloca $ \statusPtr -> do
         checkError $ Internal.recv (castPtr arrayPtr) numBytes byte (fromRank rank) (fromTag tag) comm (castPtr statusPtr)
         peek statusPtr

bcastSend :: (Storable e, Ix i, MessageArray a i e) => Comm -> Rank -> a i e -> IO ()
bcastSend comm sendRank array = do
   sendFrom array $ \arrayPtr numBytes -> do
      checkError $ Internal.bcast (castPtr arrayPtr) numBytes byte (fromRank sendRank) comm

bcastRecv :: (Storable e, Ix i, MessageArray a i e) => Comm -> Rank -> a i e -> IO ()
bcastRecv comm sendRank array = do
   recvInto array $ \arrayPtr numBytes -> do
      checkError $ Internal.bcast (castPtr arrayPtr) numBytes byte (fromRank sendRank) comm

isend, ibsend, issend :: (Storable e, Ix i, MessageArray a i e) => Comm -> Rank -> Tag -> a i e -> IO Request
isend  = isendWith Internal.isend
ibsend = isendWith Internal.ibsend
issend = isendWith Internal.issend

type ISendPrim = Ptr () -> CInt -> Datatype -> CInt -> CInt -> Comm -> Ptr (Request) -> IO CInt

isendWith :: (Storable e, Ix i, MessageArray a i e) => ISendPrim -> Comm -> Rank -> Tag -> a i e -> IO Request
isendWith send_function comm recvRank tag array = do
   sendFrom array $ \arrayPtr numBytes ->
      alloca $ \requestPtr -> do
         checkError $ send_function (castPtr arrayPtr) numBytes byte (fromRank recvRank) (fromTag tag) comm requestPtr
         peek requestPtr

{-
   At the moment we are limiting this to StorableArrays because they
   are compatible with C pointers. This means that the recieved data can
   be written directly to the array, and does not have to be copied out
   at the end. This is important for the non-blocking operation of irecv.
   It is not safe to copy the data from the C pointer until the transfer
   is complete. So any array type which required copying of data after
   receipt of the message would have to wait on complete transmission.
   It is not clear how to incorporate the waiting automatically into
   the same interface as the one below. One option is to use a Haskell
   thread to do the data copying in the "background". Another option
   is to introduce a new kind of data handle which would encapsulate the
   wait operation, and would allow the user to request the data to be
   copied when the wait was complete.
-}
irecv :: (Storable e, Ix i) => Comm -> Rank -> Tag -> StorableArray i e -> IO Request
irecv comm sendRank tag array = do
   alloca $ \requestPtr ->
      recvInto array $ \arrayPtr numBytes -> do
         checkError $ Internal.irecv (castPtr arrayPtr) numBytes byte (fromRank sendRank) (fromTag tag) comm requestPtr
         peek requestPtr

sendScatter :: (Storable e, Ix i, MessageArray a i e) => Comm -> Rank -> a i e -> a i e -> IO ()
sendScatter comm root sendArray recvArray = do
   recvInto recvArray $ \recvPtr numBytes ->
     sendFrom sendArray $ \sendPtr _ ->
       checkError $ Internal.scatter (castPtr sendPtr) numBytes byte (castPtr recvPtr) numBytes byte (fromRank root) comm

recvScatter :: (Storable e, Ix i, MessageArray a i e) => Comm -> Rank -> a i e -> IO ()
recvScatter comm root recvArray = do
   recvInto recvArray $ \recvPtr numBytes ->
     checkError $ Internal.scatter nullPtr numBytes byte (castPtr recvPtr) numBytes byte (fromRank root) comm

{-
For Scatterv/Gatherv we need arrays of "stripe lengths" and displacements.

Sometimes it would be easy for end-user to furnish those array, sometimes not.

We could steal the useful idea from mpy4pi and allow user to specify counts and displacements in several
ways - as a number, an "empty" value, or a list/array of values. Semantic is as following:

| count  | displacement | meaning                                                                                  |
|--------+--------------+------------------------------------------------------------------------------------------|
| number | nothing      | Uniform stripes of the given size, placed next to each other                             |
| number | number       | Uniform stripes of the given size, with "displ" values between them                      |
| number | vector       | Uniform stripes of the given size, at given displacements (we could check for overlap)   |
| vector | nothing      | Stripe lengths are given, compute the displacements assuming they are contiguous         |
| vector | number       | Stripe lengths are given, compute the displacements allowing "displ" values between them |
| vector | vector       | Stripes and displacements are pre-computed elsewhere                                     |


We could codify this with typeclass:

class CountsAndDisplacements a b where
  getCountsDisplacements :: (Ix i) => (i,i) -> a -> b -> (StorableArray Int Int, StorableArray Int Int)

instance CountsAndDisplacements Int Int where
  getCountsDisplacements bnds c d = striped bnds c d

instance CountsAndDisplacements Int (Maybe Int) where
  getCountsDisplacements bnds c Nothing  = striped bnds c 0
  getCountsDisplacements bnds c (Just d) = striped bnds c d

instance CountsAndDisplacements Int (StorableArray Int Int) where
  getCountsDisplacements bnds n displs  = countsOnly bnds n

instance CountsAndDisplacements (StorableArray Int Int) (Maybe Int) where
  getCountsDisplacements bnds cs Nothing  = displacementsFromCounts cs 0
  getCountsDisplacements bnds cs (Just d) = displacementsFromCounts cs d

instance CountsAndDisplacements (StorableArray Int Int) (StorableArray Int Int) where
  getCountsDisplacements bnds cs ds  = (cs,ds)

striped  = undefined
countsOnly = undefined
displacementsFromCounts = undefined

What do you think?
-}

-- Counts and displacements should be presented in ready-for-use form for speed, hence the choice of StorableArrays
-- See Serializable for other alternatives.

-- receiver needs comm rank recvcount
-- sender needs everything else
sendScatterv :: (Storable e, Ix i, MessageArray a i e) => Comm -> Rank -> a i e ->
                 StorableArray Int Int -> StorableArray Int Int -> a i e -> IO ()
sendScatterv comm root sendArray counts displacements recvArray  = do
   -- myRank <- commRank comm
   -- XXX: assert myRank == sendRank ?
   recvInto recvArray $ \recvPtr numBytes ->
     sendFrom sendArray $ \sendPtr _ ->
       withStorableArray counts $ \countsPtr ->
         withStorableArray displacements $ \displPtr ->
           checkError $ Internal.scatterv (castPtr sendPtr) (castPtr countsPtr) (castPtr displPtr) byte
                        (castPtr recvPtr) numBytes byte (fromRank root) comm

recvScatterv :: (Storable e, Ix i, MessageArray a i e) => Comm -> Rank -> a i e -> IO ()
recvScatterv comm root arr = do
   -- myRank <- commRank comm
   -- XXX: assert (myRank /= sendRank)
   recvInto arr $ \recvPtr numBytes ->
     checkError $ Internal.scatterv nullPtr nullPtr nullPtr byte (castPtr recvPtr) numBytes byte (fromRank root) comm

{-
XXX we should check that the recvArray is large enough to store:

   segmentSize * commSize
-}
recvGather :: (Storable e, Ix i, MessageArray a i e) => Comm -> Rank -> a i e -> a i e -> IO ()
recvGather comm root segment recvArray = do
   -- myRank <- commRank comm
   -- XXX: assert myRank == root
   sendFrom segment $ \sendPtr segmentBytes ->
     recvInto recvArray $ \recvPtr _ ->
       checkError $ Internal.gather (castPtr sendPtr) segmentBytes byte (castPtr recvPtr) segmentBytes byte 
                    (fromRank root) comm

sendGather :: (Storable e, Ix i, MessageArray a i e) => Comm -> Rank -> a i e -> IO ()
sendGather comm root segment = do
   -- myRank <- commRank comm
   -- XXX: assert it is /= root
   sendFrom segment $ \sendPtr segmentBytes ->
     -- the recvPtr is ignored in this case, so we can make it NULL, likewise recvCount can be 0
     checkError $ Internal.gather (castPtr sendPtr) segmentBytes byte nullPtr 0 byte (fromRank root) comm

recvGatherv :: (Storable e, Ix i, MessageArray a i e) => Comm -> Rank -> a i e ->
                StorableArray Int Int -> StorableArray Int Int -> a i e -> IO ()
recvGatherv comm root segment counts displacements recvArray = do
   -- myRank <- commRank comm
   -- XXX: assert myRank == root
   sendFrom segment $ \sendPtr segmentBytes ->
     withStorableArray counts $ \countsPtr ->
        withStorableArray displacements $ \displPtr ->
          recvInto recvArray $ \recvPtr _ ->
            checkError $ Internal.gatherv (castPtr sendPtr) segmentBytes byte (castPtr recvPtr)
                         (castPtr countsPtr) (castPtr displPtr) byte (fromRank root) comm

sendGatherv :: (Storable e, Ix i, MessageArray a i e) => Comm -> Rank -> a i e -> IO ()
sendGatherv comm root segment = do
   -- myRank <- commRank comm
   -- XXX: assert myRank == root
   sendFrom segment $ \sendPtr segmentBytes ->
     -- the recvPtr, counts and displacements are ignored in this case, so we can make it NULL
     checkError $ Internal.gatherv (castPtr sendPtr) segmentBytes byte nullPtr nullPtr nullPtr byte (fromRank root) comm

allgather :: (Storable e, Ix i, MessageArray a i e) => Comm -> a i e -> a i e -> IO ()
allgather comm sendArray recvArray = do
  sendFrom sendArray $ \sendPtr sendBytes ->
    recvInto recvArray $ \recvPtr _ -> -- Since amount sent equals amount received
      checkError $ Internal.allgather (castPtr sendPtr) sendBytes byte (castPtr recvPtr) sendBytes byte comm

allgatherv :: (Storable e, Ix i, MessageArray a i e) => Comm -> a i e -> StorableArray Int Int -> StorableArray Int Int -> a i e -> IO ()
allgatherv comm segment counts displacements recvArray = do
   sendFrom segment $ \sendPtr segmentBytes ->
     withStorableArray counts $ \countsPtr ->  
        withStorableArray displacements $ \displPtr -> 
          recvInto recvArray $ \recvPtr _ ->
            checkError $ Internal.allgatherv (castPtr sendPtr) segmentBytes byte (castPtr recvPtr) (castPtr countsPtr) (castPtr displPtr) byte comm
             
alltoall :: forall a i e.(Storable e, Ix i, MessageArray a i e) => Comm -> a i e -> Int -> a i e -> IO ()
alltoall comm sendArray sendCount recvArray = do
  let elemSize = sizeOf (undefined :: e)
      sendBytes = cIntConv (elemSize * sendCount)
  sendFrom sendArray $ \sendPtr _ ->
    recvInto recvArray $ \recvPtr _ -> -- Since amount sent equals amount received
      checkError $ Internal.alltoall (castPtr sendPtr) sendBytes byte (castPtr recvPtr) sendBytes byte comm

class (Storable e, Ix i) => MessageArray array i e where
   recvInto :: array i e -> (Ptr e -> CInt -> IO a) -> IO a
   sendFrom :: array i e -> (Ptr e -> CInt -> IO a) -> IO a

instance (Ix i, Storable e) => MessageArray StorableArray i e where
   recvInto = withStorableArrayAndSize
   sendFrom = withStorableArrayAndSize

withStorableArrayAndSize :: forall i e a. (Storable e, Ix i) => StorableArray i e -> (Ptr e -> CInt -> IO a) -> IO a
withStorableArrayAndSize arr f = do
   rSize <- rangeSize <$> getBounds arr
   let numBytes = cIntConv (rSize * sizeOf (undefined :: e))
   withStorableArray arr $ \ptr -> f ptr numBytes

instance (Ix i, Storable e) => MessageArray IOArray i e where
   recvInto = recvWithMArrayAndSize
   sendFrom = sendWithMArrayAndSize

{-
XXX this does not work, because the compiler can't infer from (Storable e, Ix i)
    IOUArray i e, because only a select number of types can be elements of
    unboxed arrays. So sadly it appears that we have to enumerate them.

instance (Storable e, Ix i) => MessageArray IOUArray i e where
   recvInto = recvWithMArrayAndSize
   sendFrom = sendWithMArrayAndSize
-}

instance Ix i => MessageArray IOUArray i Bool where
   recvInto = recvWithMArrayAndSize
   sendFrom = sendWithMArrayAndSize

instance Ix i => MessageArray IOUArray i Char where
   recvInto = recvWithMArrayAndSize
   sendFrom = sendWithMArrayAndSize

instance Ix i => MessageArray IOUArray i Double where
   recvInto = recvWithMArrayAndSize
   sendFrom = sendWithMArrayAndSize

instance Ix i => MessageArray IOUArray i Float where
   recvInto = recvWithMArrayAndSize
   sendFrom = sendWithMArrayAndSize

instance Ix i => MessageArray IOUArray i Int where
   recvInto = recvWithMArrayAndSize
   sendFrom = sendWithMArrayAndSize

instance Ix i => MessageArray IOUArray i Int8 where
   recvInto = recvWithMArrayAndSize
   sendFrom = sendWithMArrayAndSize

instance Ix i => MessageArray IOUArray i Int16 where
   recvInto = recvWithMArrayAndSize
   sendFrom = sendWithMArrayAndSize

instance Ix i => MessageArray IOUArray i Int32 where
   recvInto = recvWithMArrayAndSize
   sendFrom = sendWithMArrayAndSize

instance Ix i => MessageArray IOUArray i Int64 where
   recvInto = recvWithMArrayAndSize
   sendFrom = sendWithMArrayAndSize

instance Ix i => MessageArray IOUArray i Word where
   recvInto = recvWithMArrayAndSize
   sendFrom = sendWithMArrayAndSize

instance Ix i => MessageArray IOUArray i Word8 where
   recvInto = recvWithMArrayAndSize
   sendFrom = sendWithMArrayAndSize

instance Ix i => MessageArray IOUArray i Word16 where
   recvInto = recvWithMArrayAndSize
   sendFrom = sendWithMArrayAndSize

instance Ix i => MessageArray IOUArray i Word32 where
   recvInto = recvWithMArrayAndSize
   sendFrom = sendWithMArrayAndSize

instance Ix i => MessageArray IOUArray i Word64 where
   recvInto = recvWithMArrayAndSize
   sendFrom = sendWithMArrayAndSize

instance Ix i => MessageArray IOUArray i (StablePtr a) where
   recvInto = recvWithMArrayAndSize
   sendFrom = sendWithMArrayAndSize

instance Ix i => MessageArray IOUArray i (Ptr a) where
   recvInto = recvWithMArrayAndSize
   sendFrom = sendWithMArrayAndSize

instance Ix i => MessageArray IOUArray i (FunPtr a) where
   recvInto = recvWithMArrayAndSize
   sendFrom = sendWithMArrayAndSize

recvWithMArrayAndSize :: forall i e r a. (Storable e, Ix i, MArray a e IO) => a i e -> (Ptr e -> CInt -> IO r) -> IO r
recvWithMArrayAndSize array f = do
   bounds <- getBounds array
   let numElements = rangeSize bounds
   let elementSize = sizeOf (undefined :: e)
   let numBytes = cIntConv (numElements * elementSize)
   allocaArray numElements $ \ptr -> do
      result <- f ptr numBytes
      fillArrayFromPtr (range bounds) numElements ptr array
      return result

sendWithMArrayAndSize :: forall i e r a. (Storable e, Ix i, MArray a e IO) => a i e -> (Ptr e -> CInt -> IO r) -> IO r
sendWithMArrayAndSize array f = do
   elements <- getElems array
   bounds <- getBounds array
   let numElements = rangeSize bounds
   let elementSize = sizeOf (undefined :: e)
   let numBytes = cIntConv (numElements * elementSize)
   withArray elements $ \ptr -> f ptr numBytes

-- XXX I wonder if this can be written without the intermediate list?
-- Maybe GHC can elimiate it. We should look at the generated compiled
-- code to see how well the loop is handled.
fillArrayFromPtr :: (MArray a e IO, Storable e, Ix i) => [i] -> Int -> Ptr e -> a i e -> IO ()
fillArrayFromPtr indices numElements startPtr array = do
   elems <- peekArray numElements startPtr
   mapM_ (\(index, element) -> writeArray array index element) (zip indices elems)
