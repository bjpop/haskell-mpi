{-# LANGUAGE FlexibleContexts, FlexibleInstances, ScopedTypeVariables, UndecidableInstances, CPP #-}

module Control.Parallel.MPI.Storable
   ( SendFrom (..)
   , RecvInto (..)
   , Repr (..)
   , send
   , ssend
   , bsend
   , rsend
   , recv
   , bcastSend
   , bcastRecv
   , isend
   , ibsend
   , issend
   , isendPtr
   , ibsendPtr
   , issendPtr
   , irecv
   , irecvPtr
   , waitall
   , scatterSend
   , scatterRecv
   , scattervSend
   , scattervRecv
   , gatherSend
   , gatherRecv
   , gathervSend
   , gathervRecv
   , allgather
   , allgatherv
   , alltoall
   , alltoallv
   , sendReduce
   , recvReduce
   , allreduce
   , reduceScatter
   , opCreate
   , intoNewArray
   , intoNewArray_
   , intoNewVal
   , intoNewVal_
   , intoNewBS
   , intoNewBS_
   , module Data.Word
   ) where

#include "MachDeps.h"

import C2HS
import Data.Array.Base (unsafeNewArray_)
import Data.Array.IO
import Data.Array.Storable
import Control.Applicative ((<$>))
import Data.ByteString.Unsafe as BS
import qualified Data.ByteString as BS
import qualified Control.Parallel.MPI.Internal as Internal
import Control.Parallel.MPI
import Control.Parallel.MPI.Exception (checkError)
import Data.Int()
import Data.Word

{-

In-place receive vs new array allocation for Storable Array
-----------------------------------------------------------
When using StorableArray API in tight numeric loops, it is best to
reuse existing arrays and avoid penalties incurred by
allocation/deallocation of memory. Which is why destinations/receive
buffers in StorableArray API are specified exclusively as
(StorableArray i e).

If you'd rather allocate new array for a particular operation, you
could use withNewArray/withNewArray_:

Instead of (recv comm rank tag arr) you would write 
(arr <- withNewArray bounds $ recv comm rank tag), and new array would
be allocated, supplied as the target of the (recv) operation and
returned to you.

You could easily write your own convenience wrappers similar to
withNewArray. For example, you could create wrapper that would take an
array size as a simple number instead of range.

-}

-- | if the user wants to call scattervRecv for the first time without
-- already having allocated the array, then they can call it like so:
--
-- (array,_) <- intoNewArray range $ scattervRecv root comm
--
-- and thereafter they can call it like so:
--
--  scattervRecv root comm array
intoNewArray :: (Ix i, MArray a e m, RecvInto (a i e)) => (i, i) -> (a i e -> m r) -> m (a i e, r)
intoNewArray range f = do
  arr <- unsafeNewArray_ range -- New, uninitialized array, According to http://hackage.haskell.org/trac/ghc/ticket/3586
                               -- should be faster than newArray_
  res <- f arr
  return (arr, res)

-- | Same as withRange, but discards the result of the processor function
intoNewArray_ :: (Ix i, MArray a e m, RecvInto (a i e)) => (i, i) -> (a i e -> m r) -> m (a i e)
intoNewArray_ range f = do
  arr <- unsafeNewArray_ range
  _ <- f arr
  return arr

send, bsend, ssend, rsend :: (SendFrom v) => Comm -> Rank -> Tag -> v -> IO ()
send  = sendWith Internal.send
bsend = sendWith Internal.bsend
ssend = sendWith Internal.ssend
rsend = sendWith Internal.rsend

type SendPrim = Ptr () -> CInt -> Datatype -> Rank -> Tag -> Comm -> IO ()

sendWith :: (SendFrom v) => SendPrim -> Comm -> Rank -> Tag -> v -> IO ()
sendWith send_function comm rank tag val = do
   sendFrom val $ \valPtr numBytes dtype -> do
      send_function (castPtr valPtr) numBytes dtype (fromRank rank) (fromTag tag) comm

recv :: (RecvInto v) => Comm -> Rank -> Tag -> v -> IO Status
recv comm rank tag arr = do
   recvInto arr $ \valPtr numBytes dtype ->
      alloca $ \statusPtr -> do
         checkError $ Internal.recv (castPtr valPtr) numBytes dtype (fromRank rank) (fromTag tag) comm (castPtr statusPtr)
         peek statusPtr

bcastSend :: (SendFrom v) => Comm -> Rank -> v -> IO ()
bcastSend comm sendRank val = do
   sendFrom val $ \valPtr numBytes dtype -> do
      checkError $ Internal.bcast (castPtr valPtr) numBytes dtype (fromRank sendRank) comm

bcastRecv :: (RecvInto v) => Comm -> Rank -> v -> IO ()
bcastRecv comm sendRank val = do
   recvInto val $ \valPtr numBytes dtype -> do
      checkError $ Internal.bcast (castPtr valPtr) numBytes dtype (fromRank sendRank) comm

isend, ibsend, issend :: (SendFrom v) => Comm -> Rank -> Tag -> v -> IO Request
isend  = isendWith Internal.isend
ibsend = isendWith Internal.ibsend
issend = isendWith Internal.issend

type ISendPrim = Ptr () -> CInt -> Datatype -> Rank -> Tag -> Comm -> IO (Request)

isendWith :: (SendFrom v) => ISendPrim -> Comm -> Rank -> Tag -> v -> IO Request
isendWith send_function comm recvRank tag val = do
  sendFrom val $ \valPtr numBytes dtype -> do
    send_function valPtr numBytes dtype recvRank tag comm


isendPtr, ibsendPtr, issendPtr :: (SendFrom v) => Comm -> Rank -> Tag -> Ptr Request -> v -> IO ()
isendPtr  = isendWithPtr Internal.isendPtr
ibsendPtr = isendWithPtr Internal.ibsendPtr
issendPtr = isendWithPtr Internal.issendPtr

type ISendPtrPrim = Ptr () -> CInt -> Datatype -> Rank -> Tag -> Comm -> Ptr Request -> IO ()
isendWithPtr :: (SendFrom v) => ISendPtrPrim -> Comm -> Rank -> Tag -> Ptr Request -> v -> IO ()
isendWithPtr send_function comm recvRank tag requestPtr val = do
   sendFrom val $ \valPtr numBytes dtype ->
     send_function (castPtr valPtr) numBytes dtype (fromRank recvRank) (fromTag tag) comm requestPtr

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

-- Pointer to Request is provided by called. Usefull for filling arrays of Requests for further consumption
-- by waitall
irecvPtr :: (Storable e, Ix i, Repr e) => Comm -> Rank -> Tag -> Ptr Request -> StorableArray i e -> IO ()
irecvPtr comm sendRank tag requestPtr recvVal = do
  recvInto recvVal $ \recvPtr recvElements recvType -> do
    checkError $ Internal.irecvPtr (castPtr recvPtr) recvElements recvType (fromRank sendRank) (fromTag tag) comm requestPtr

irecv :: (Storable e, Ix i, Repr e) => Comm -> Rank -> Tag -> StorableArray i e -> IO Request
irecv comm sendRank tag recvVal = do
   alloca $ \requestPtr -> do
     irecvPtr comm sendRank tag requestPtr recvVal
     peek requestPtr

waitall :: StorableArray Int Request -> StorableArray Int Status -> IO ()
waitall requests statuses = do
  cnt <- rangeSize <$> getBounds requests
  withStorableArray requests $ \reqs ->
    withStorableArray statuses $ \stats ->
      checkError $ Internal.waitall (cIntConv cnt) (castPtr reqs) (castPtr stats)

scatterSend :: (SendFrom v1, RecvInto v2) => Comm -> Rank -> v1 -> v2 -> IO ()
scatterSend comm root sendVal recvVal = do
   recvInto recvVal $ \recvPtr recvElements recvType ->
     sendFrom sendVal $ \sendPtr _ _ ->
       checkError $ Internal.scatter (castPtr sendPtr) recvElements recvType (castPtr recvPtr) recvElements recvType (fromRank root) comm

scatterRecv :: (RecvInto v) => Comm -> Rank -> v -> IO ()
scatterRecv comm root recvVal = do
   recvInto recvVal $ \recvPtr recvElements recvType ->
     checkError $ Internal.scatter nullPtr 0 byte (castPtr recvPtr) recvElements recvType (fromRank root) comm

-- Counts and displacements should be presented in ready-for-use form for speed, hence the choice of StorableArrays
scattervSend :: (SendFrom v1, RecvInto v2) => Comm -> Rank -> v1 ->
                 StorableArray Int CInt -> StorableArray Int CInt -> v2 -> IO ()
scattervSend comm root sendVal counts displacements recvVal  = do
   -- myRank <- commRank comm
   -- XXX: assert myRank == sendRank ?
   recvInto recvVal $ \recvPtr recvElements recvType ->
     sendFrom sendVal $ \sendPtr _ sendType->
       withStorableArray counts $ \countsPtr ->
         withStorableArray displacements $ \displPtr ->
           checkError $ Internal.scatterv (castPtr sendPtr) (castPtr countsPtr) (castPtr displPtr) sendType
                        (castPtr recvPtr) recvElements recvType (fromRank root) comm

scattervRecv :: (RecvInto v) => Comm -> Rank -> v -> IO ()
scattervRecv comm root arr = do
   -- myRank <- commRank comm
   -- XXX: assert (myRank /= sendRank)
   recvInto arr $ \recvPtr recvElements recvType ->
     checkError $ Internal.scatterv nullPtr nullPtr nullPtr byte (castPtr recvPtr) recvElements recvType (fromRank root) comm

{-
XXX we should check that the recvArray is large enough to store:

   segmentSize * commSize
-}
gatherRecv :: (SendFrom v1, RecvInto v2) => Comm -> Rank -> v1 -> v2 -> IO ()
gatherRecv comm root segment recvVal = do
   -- myRank <- commRank comm
   -- XXX: assert myRank == root
   sendFrom segment $ \sendPtr sendElements sendType ->
     recvInto recvVal $ \recvPtr _ _ ->
       checkError $ Internal.gather (castPtr sendPtr) sendElements sendType (castPtr recvPtr) sendElements sendType 
                    (fromRank root) comm

gatherSend :: (SendFrom v) => Comm -> Rank -> v -> IO ()
gatherSend comm root segment = do
   -- myRank <- commRank comm
   -- XXX: assert it is /= root
   sendFrom segment $ \sendPtr sendElements sendType ->
     -- the recvPtr is ignored in this case, so we can make it NULL, likewise recvCount can be 0
     checkError $ Internal.gather (castPtr sendPtr) sendElements sendType nullPtr 0 byte (fromRank root) comm

gathervRecv :: (SendFrom v1, RecvInto v2) => Comm -> Rank -> v1 ->
                StorableArray Int CInt -> StorableArray Int CInt -> v2 -> IO ()
gathervRecv comm root segment counts displacements recvVal = do
   -- myRank <- commRank comm
   -- XXX: assert myRank == root
   sendFrom segment $ \sendPtr sendElements sendType ->
     withStorableArray counts $ \countsPtr ->
        withStorableArray displacements $ \displPtr ->
          recvInto recvVal $ \recvPtr _ recvType->
            checkError $ Internal.gatherv (castPtr sendPtr) sendElements sendType (castPtr recvPtr)
                         (castPtr countsPtr) (castPtr displPtr) recvType (fromRank root) comm

gathervSend :: (SendFrom v) => Comm -> Rank -> v -> IO ()
gathervSend comm root segment = do
   -- myRank <- commRank comm
   -- XXX: assert myRank == root
   sendFrom segment $ \sendPtr sendElements sendType ->
     -- the recvPtr, counts and displacements are ignored in this case, so we can make it NULL
     checkError $ Internal.gatherv (castPtr sendPtr) sendElements sendType nullPtr nullPtr nullPtr byte (fromRank root) comm

allgather :: (SendFrom v1, RecvInto v2) => Comm -> v1 -> v2 -> IO ()
allgather comm sendVal recvVal = do
  sendFrom sendVal $ \sendPtr sendElements sendType ->
    recvInto recvVal $ \recvPtr _ _ -> -- Since amount sent equals amount received
      checkError $ Internal.allgather (castPtr sendPtr) sendElements sendType (castPtr recvPtr) sendElements sendType comm

allgatherv :: (SendFrom v1, RecvInto v2) => Comm -> v1 -> StorableArray Int CInt -> StorableArray Int CInt -> v2 -> IO ()
allgatherv comm segment counts displacements recvVal = do
   sendFrom segment $ \sendPtr sendElements sendType ->
     withStorableArray counts $ \countsPtr ->  
        withStorableArray displacements $ \displPtr -> 
          recvInto recvVal $ \recvPtr _ recvType ->
            checkError $ Internal.allgatherv (castPtr sendPtr) sendElements sendType (castPtr recvPtr) (castPtr countsPtr) (castPtr displPtr) recvType comm
             
-- XXX: when sending arrays, we can not measure the size of the array element with sizeOf here without
-- breaking the abstraction. Hence for now `sendCount' should be treated as "count of the underlying MPI
-- representation elements that have to be sent to each process". User is expected to know that type and its size.
-- XXX: we should probably take representation scale into account here
alltoall :: (SendFrom v1, RecvInto v2) => Comm -> v1 -> Int -> v2 -> IO ()
alltoall comm sendVal sendCount recvVal = do
  let sendCount_ = cIntConv sendCount
  sendFrom sendVal $ \sendPtr _ sendType ->
    recvInto recvVal $ \recvPtr _ _ -> -- Since amount sent must equal amount received
      checkError $ Internal.alltoall (castPtr sendPtr) sendCount_ sendType (castPtr recvPtr) sendCount_ sendType comm

alltoallv :: (SendFrom v1, RecvInto v2) => Comm -> v1 -> StorableArray Int CInt -> StorableArray Int CInt -> StorableArray Int CInt -> StorableArray Int CInt -> v2 -> IO ()
alltoallv comm sendVal sendCounts sendDisplacements recvCounts recvDisplacements recvVal = do
  sendFrom sendVal $ \sendPtr _ sendType ->
    recvInto recvVal $ \recvPtr _ recvType ->
      withStorableArray sendCounts $ \sendCountsPtr ->
        withStorableArray sendDisplacements $ \sendDisplPtr ->
          withStorableArray recvCounts $ \recvCountsPtr ->
            withStorableArray recvDisplacements $ \recvDisplPtr ->
              checkError $ Internal.alltoallv (castPtr sendPtr) (castPtr sendCountsPtr) (castPtr sendDisplPtr) sendType
                                              (castPtr recvPtr) (castPtr recvCountsPtr) (castPtr recvDisplPtr) recvType comm
  
sendReduce :: SendFrom v => Comm -> Rank -> Operation -> v -> IO ()
sendReduce comm root op sendVal = do
  sendFrom sendVal $ \sendPtr sendElements sendType ->
    checkError $ Internal.reduce (castPtr sendPtr) nullPtr sendElements sendType op (fromRank root) comm

recvReduce :: (SendFrom v, RecvInto v) => Comm -> Rank -> Operation -> v -> v -> IO ()
recvReduce comm root op sendVal recvVal =
  sendFrom sendVal $ \sendPtr sendElements sendType ->
    recvInto recvVal $ \recvPtr _ _ ->
      checkError $ Internal.reduce (castPtr sendPtr) (castPtr recvPtr) sendElements sendType op (fromRank root) comm

allreduce :: (SendFrom v, RecvInto v) => Comm -> Operation -> v -> v -> IO ()
allreduce comm op sendVal recvVal = 
  sendFrom sendVal $ \sendPtr sendElements sendType ->
    recvInto recvVal $ \recvPtr _ _ ->
      checkError $ Internal.allreduce (castPtr sendPtr) (castPtr recvPtr) sendElements sendType op comm

reduceScatter :: (SendFrom v, RecvInto v) => Comm -> Operation -> StorableArray Int CInt -> v -> v -> IO ()
reduceScatter comm op counts sendVal recvVal =
  sendFrom sendVal $ \sendPtr _ sendType ->
    recvInto recvVal $ \recvPtr _ _ ->
      withStorableArray counts $ \countsPtr ->
      checkError $ Internal.reduceScatter (castPtr sendPtr) (castPtr recvPtr) (castPtr countsPtr) sendType op comm

class Repr e where
  -- How many elements of given datatype do we need to represent given
  -- type in MPI transfers
  representation :: e -> (Int, Datatype)
  
instance Repr Bool where
  representation _ = (1,unsigned)

instance Repr Int where
#if SIZEOF_HSINT == 4  
  representation _ = (1,int)
#elif SIZEOF_HSINT == 8
  representation _ = (1,longLong)
#else
#error Haskell MPI bindings not tested on architecture where size of Haskell Int is not 4 or 8
#endif

instance Repr Int8 where
   representation _ = (1,byte)
instance Repr Int16 where
   representation _ = (1,short)
instance Repr Int32 where
   representation _ = (1,int)
instance Repr Int64 where
   representation _ = (1,longLong)
instance Repr CInt where
  representation _ = (1,int)

instance Repr Word where
#if SIZEOF_HSINT == 4  
  representation _ = (1,unsigned)
#else
  representation _ = (1,unsignedLongLong)
#endif

instance Repr Word8 where
  representation _ = (1,byte)
instance Repr Word16 where
  representation _ = (1,unsignedShort)
instance Repr Word32 where
  representation _ = (1,unsigned)
instance Repr Word64 where
  representation _ = (1,unsignedLongLong)

instance Repr Char where
  representation _ = (1,wchar)
instance Repr CChar where
  representation _ = (1,char)

instance Repr Double where
  representation _ = (1,double)
instance Repr Float where
  representation _ = (1,float)

instance Repr e => Repr (StorableArray i e) where
  representation _ = representation (undefined::e)

instance Repr e => Repr (IOArray i e) where
  representation _ = representation (undefined::e)

instance Repr e => Repr (IOUArray i e) where
  representation _ = representation (undefined::e)

-- Note that 'e' is not bound by the typeclass, so all kinds of foul play
-- are possible. However, since MPI declares all buffers as 'void*' anyway, 
-- we are not making life all _that_ unsafe with this
class SendFrom v where
   sendFrom :: v -> (Ptr e -> CInt -> Datatype -> IO a) -> IO a

class RecvInto v where
   recvInto :: v -> (Ptr e -> CInt -> Datatype -> IO a) -> IO a

-- Sending from a single Storable values
instance SendFrom CInt where
  sendFrom = sendFromSingleValue
instance SendFrom Int where
  sendFrom = sendFromSingleValue
instance SendFrom Int8 where
  sendFrom = sendFromSingleValue
instance SendFrom Int16 where
  sendFrom = sendFromSingleValue
instance SendFrom Int32 where
  sendFrom = sendFromSingleValue
instance SendFrom Int64 where
  sendFrom = sendFromSingleValue
instance SendFrom Word where
  sendFrom = sendFromSingleValue
instance SendFrom Word8 where
  sendFrom = sendFromSingleValue
instance SendFrom Word16 where
  sendFrom = sendFromSingleValue
instance SendFrom Word32 where
  sendFrom = sendFromSingleValue
instance SendFrom Word64 where
  sendFrom = sendFromSingleValue
instance SendFrom Bool where
  sendFrom = sendFromSingleValue
instance SendFrom Float where
  sendFrom = sendFromSingleValue
instance SendFrom Double where
  sendFrom = sendFromSingleValue
instance SendFrom Char where
  sendFrom = sendFromSingleValue
instance SendFrom CChar where
  sendFrom = sendFromSingleValue

sendFromSingleValue :: (Repr v, Storable v) => v -> (Ptr e -> CInt -> Datatype -> IO a) -> IO a
sendFromSingleValue v f = do
  alloca $ \ptr -> do
    poke ptr v
    let (1, dtype) = representation v
    f (castPtr ptr) (1::CInt) dtype

-- Sending-receiving arrays of such values
instance (Storable e, Repr e, Ix i) => SendFrom (StorableArray i e) where
  sendFrom = withStorableArrayAndSize

instance (Storable e, Repr e, Ix i) => RecvInto (StorableArray i e) where
  recvInto = withStorableArrayAndSize

withStorableArrayAndSize :: forall a i e z.(Repr e, Storable e, Ix i) => StorableArray i e -> (Ptr z -> CInt -> Datatype -> IO a) -> IO a
withStorableArrayAndSize arr f = do
   rSize <- rangeSize <$> getBounds arr
   let (scale, dtype) = (representation (undefined :: StorableArray i e))
       numElements = cIntConv (rSize * scale)
   withStorableArray arr $ \ptr -> f (castPtr ptr) numElements dtype

-- Same, for IOArray
instance (Storable e, Repr (IOArray i e), Ix i) => SendFrom (IOArray i e) where
  sendFrom = sendWithMArrayAndSize
instance (Storable e, Repr (IOArray i e), Ix i) => RecvInto (IOArray i e) where
  recvInto = recvWithMArrayAndSize

recvWithMArrayAndSize :: forall i e r a z. (Storable e, Ix i, MArray a e IO, Repr (a i e)) => a i e -> (Ptr z -> CInt -> Datatype -> IO r) -> IO r
recvWithMArrayAndSize array f = do
   bounds <- getBounds array
   let (scale, dtype) = representation (undefined :: a i e)
       numElements = cIntConv $ rangeSize bounds * scale
   allocaArray (rangeSize bounds) $ \ptr -> do
      result <- f (castPtr ptr) numElements dtype
      fillArrayFromPtr (range bounds) (rangeSize bounds) ptr array
      return result

sendWithMArrayAndSize :: forall i e r a z. (Storable e, Ix i, MArray a e IO, Repr (a i e)) => a i e -> (Ptr z -> CInt -> Datatype -> IO r) -> IO r
sendWithMArrayAndSize array f = do
   elements <- getElems array
   bounds <- getBounds array
   let (scale, dtype) = representation (undefined :: a i e)
       numElements = cIntConv $ rangeSize bounds * scale
   withArray elements $ \ptr -> f (castPtr ptr) numElements dtype

-- XXX I wonder if this can be written without the intermediate list?
-- Maybe GHC can elimiate it. We should look at the generated compiled
-- code to see how well the loop is handled.
fillArrayFromPtr :: (MArray a e IO, Storable e, Ix i) => [i] -> Int -> Ptr e -> a i e -> IO ()
fillArrayFromPtr indices numElements startPtr array = do
   elems <- peekArray numElements startPtr
   mapM_ (\(index, element) -> writeArray array index element ) (zip indices elems)

-- ByteString
instance SendFrom BS.ByteString where
  sendFrom = sendWithByteStringAndSize

sendWithByteStringAndSize :: BS.ByteString -> (Ptr z -> CInt -> Datatype -> IO a) -> IO a
sendWithByteStringAndSize bs f = do
  unsafeUseAsCStringLen bs $ \(bsPtr,len) -> f (castPtr bsPtr) (cIntConv len) byte

-- Pointers to storable with known on-wire representation
instance (Storable e, Repr e) => RecvInto (Ptr e) where
  recvInto = recvIntoElemPtr (representation (undefined :: e))
    where
      recvIntoElemPtr (cnt,datatype) p f = f (castPtr p) (cIntConv cnt) datatype

instance (Storable e, Repr e) => RecvInto (Ptr e, Int) where
  recvInto = recvIntoVectorPtr (representation (undefined :: e))
    where
      recvIntoVectorPtr (scale, datatype) (p,len) f = f (castPtr p) (cIntConv (len * scale) :: CInt) datatype

intoNewVal :: (Storable e) => (Ptr e -> IO r) -> IO (e, r)
intoNewVal f = do
  alloca $ \ptr -> do
    res <- f ptr
    val <- peek ptr
    return (val, res)

intoNewVal_ :: (Storable e) => (Ptr e -> IO r) -> IO e
intoNewVal_ f = do
  (val, _) <- intoNewVal f
  return val

-- Receiving into new bytestrings
intoNewBS :: Integral a => a -> ((Ptr CChar,Int) -> IO r) -> IO (BS.ByteString, r)
intoNewBS len f = do
  let l = fromIntegral len
  allocaBytes l $ \ptr -> do
    res <- f (ptr, l)
    bs <- BS.packCStringLen (ptr, l)
    return (bs, res)

intoNewBS_ :: Integral a => a -> ((Ptr CChar,Int) -> IO r) -> IO BS.ByteString
intoNewBS_ len f = do
  (bs, _) <- intoNewBS len f
  return bs

opCreate :: Storable t => Bool -> (FunPtr (Ptr t -> Ptr t -> Ptr CInt -> Ptr Datatype -> IO ())) -> IO Operation
opCreate commute f = do
  alloca $ \ptr -> do
    checkError $ Internal.opCreate (castFunPtr f) (cIntConv $ fromEnum commute) ptr
    peek (castPtr ptr)
