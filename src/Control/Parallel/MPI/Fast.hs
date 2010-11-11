{-# LANGUAGE FlexibleContexts, FlexibleInstances, ScopedTypeVariables, UndecidableInstances, CPP #-}

-----------------------------------------------------------------------------
{- |
Module      : Control.Parallel.MPI.Fast
Copyright   : (c) 2010 Bernie Pope, Dmitry Astapov
License     : BSD-style
Maintainer  : florbitous@gmail.com
Stability   : experimental
Portability : ghc

This module provides MPI functionality for arbitrary Haskell types that could be
represented by corresponding MPI types without additional conversion or serialization,
which allows fast application of MPI operations.

TODO: expand

Full range of point-to-point and collective operation is supported, including for reduce and similar operations.

Below is a small but complete MPI program utilising this Module. Process 0 sends the array of @Int@s
process 1. Process 1 receives the message and prints it
to standard output. It assumes that there are at least 2 MPI processes
available. Further examples in this module would provide different implementation of
@process@ function.

@
\{\-\# LANGUAGE ScopedTypeVariables \#\-\}
module Main where

import Control.Parallel.MPI.Fast ('mpi', 'commRank', 'commWorld', 'unitTag', 'send', 'recv', 'intoNewArray')
import Data.Array.Storable

type ArrMsg = StorableArray Int Int

arrMsg :: IO (StorableArray Int Int)
arrMsg = newListArray (1,10) [1..10]

main :: IO ()
main = 'mpi' $ do
   rank <- 'commRank' 'commWorld'
   process rank

process :: 'Rank' -> IO ()
process rank
   | rank == 0 = do sendMsg <- arrMsg
                    'send' 'commWorld' 1 2 sendMsg
   | rank == 1 = do (recvMsg::ArrMsg, status) <- 'intoNewArray' (1,10) $ 'recv' 'commWorld' 0 2
                    els <- getElems recvMsg
                    putStrLn $ \"Got message: \" ++ show els
   | otherwise = return ()
@
-}
-----------------------------------------------------------------------------
module Control.Parallel.MPI.Fast
   ( 
     -- * Mapping between Haskell and MPI types
     Repr (..)
     
     -- * Treating Haskell values as send or receive buffers
   , SendFrom (..)
   , RecvInto (..)

     -- * On-the-fly buffer allocation helpers
   , intoNewArray
   , intoNewArray_
   , intoNewVal
   , intoNewVal_
   , intoNewBS
   , intoNewBS_
     
     -- * Point-to-point operations.
     -- ** Blocking.
   , send
   , bsend
   , ssend
   , rsend
   , recv
     -- ** Non-blocking.
   , isend
   , ibsend
   , issend
   , irecv
   , isendPtr
   , ibsendPtr
   , issendPtr
   , irecvPtr
   , waitall
   -- * Collective operations.
   -- ** One-to-all.
   , bcastSend
   , bcastRecv
   , scatterSend
   , scatterRecv
   , scattervSend
   , scattervRecv
   -- ** All-to-one.
   , gatherSend
   , gatherRecv
   , gathervSend
   , gathervRecv
   , reduceSend
   , reduceRecv
   -- ** All-to-all.
   , allgather
   , allgatherv
   , alltoall
   , alltoallv
   , allreduce
   , reduceScatter
   , opCreate
   , Internal.opFree
     
   , module Data.Word
   , module Control.Parallel.MPI.Base
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
import Control.Parallel.MPI.Base
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
      send_function (castPtr valPtr) numBytes dtype rank tag comm

recv :: (RecvInto v) => Comm -> Rank -> Tag -> v -> IO Status
recv comm rank tag arr = do
   recvInto arr $ \valPtr numBytes dtype ->
      Internal.recv (castPtr valPtr) numBytes dtype rank tag comm

bcastSend :: (SendFrom v) => Comm -> Rank -> v -> IO ()
bcastSend comm sendRank val = do
   sendFrom val $ \valPtr numBytes dtype -> do
      Internal.bcast (castPtr valPtr) numBytes dtype sendRank comm

bcastRecv :: (RecvInto v) => Comm -> Rank -> v -> IO ()
bcastRecv comm sendRank val = do
   recvInto val $ \valPtr numBytes dtype -> do
      Internal.bcast (castPtr valPtr) numBytes dtype sendRank comm

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
     send_function (castPtr valPtr) numBytes dtype recvRank tag comm requestPtr

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

-- Pointer to Request is provided by caller. Usefull for filling arrays of Requests for further consumption
-- by waitall
irecvPtr :: (Storable e, Ix i, Repr e) => Comm -> Rank -> Tag -> Ptr Request -> StorableArray i e -> IO ()
irecvPtr comm sendRank tag requestPtr recvVal = do
  recvInto recvVal $ \recvPtr recvElements recvType -> do
    Internal.irecvPtr (castPtr recvPtr) recvElements recvType sendRank tag comm requestPtr

irecv :: (Storable e, Ix i, Repr e) => Comm -> Rank -> Tag -> StorableArray i e -> IO Request
irecv comm sendRank tag recvVal = do
   recvInto recvVal $ \recvPtr recvElements recvType -> do
      Internal.irecv (castPtr recvPtr) recvElements recvType sendRank tag comm

waitall :: StorableArray Int Request -> StorableArray Int Status -> IO ()
waitall requests statuses = do
  cnt <- rangeSize <$> getBounds requests
  withStorableArray requests $ \reqs ->
    withStorableArray statuses $ \stats ->
      Internal.waitall (cIntConv cnt) (castPtr reqs) (castPtr stats)

scatterSend :: (SendFrom v1, RecvInto v2) => Comm -> Rank -> v1 -> v2 -> IO ()
scatterSend comm root sendVal recvVal = do
   recvInto recvVal $ \recvPtr recvElements recvType ->
     sendFrom sendVal $ \sendPtr _ _ ->
       Internal.scatter (castPtr sendPtr) recvElements recvType (castPtr recvPtr) recvElements recvType root comm

scatterRecv :: (RecvInto v) => Comm -> Rank -> v -> IO ()
scatterRecv comm root recvVal = do
   recvInto recvVal $ \recvPtr recvElements recvType ->
     Internal.scatter nullPtr 0 byte (castPtr recvPtr) recvElements recvType root comm

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
           Internal.scatterv (castPtr sendPtr) countsPtr displPtr sendType
                             (castPtr recvPtr) recvElements recvType root comm

scattervRecv :: (RecvInto v) => Comm -> Rank -> v -> IO ()
scattervRecv comm root arr = do
   -- myRank <- commRank comm
   -- XXX: assert (myRank /= sendRank)
   recvInto arr $ \recvPtr recvElements recvType ->
     Internal.scatterv nullPtr nullPtr nullPtr byte (castPtr recvPtr) recvElements recvType root comm

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
       Internal.gather (castPtr sendPtr) sendElements sendType (castPtr recvPtr) sendElements sendType root comm

gatherSend :: (SendFrom v) => Comm -> Rank -> v -> IO ()
gatherSend comm root segment = do
   -- myRank <- commRank comm
   -- XXX: assert it is /= root
   sendFrom segment $ \sendPtr sendElements sendType ->
     -- the recvPtr is ignored in this case, so we can make it NULL, likewise recvCount can be 0
     Internal.gather (castPtr sendPtr) sendElements sendType nullPtr 0 byte root comm

gathervRecv :: (SendFrom v1, RecvInto v2) => Comm -> Rank -> v1 ->
                StorableArray Int CInt -> StorableArray Int CInt -> v2 -> IO ()
gathervRecv comm root segment counts displacements recvVal = do
   -- myRank <- commRank comm
   -- XXX: assert myRank == root
   sendFrom segment $ \sendPtr sendElements sendType ->
     withStorableArray counts $ \countsPtr ->
        withStorableArray displacements $ \displPtr ->
          recvInto recvVal $ \recvPtr _ recvType->
            Internal.gatherv (castPtr sendPtr) sendElements sendType 
                             (castPtr recvPtr) countsPtr displPtr recvType 
                             root comm

gathervSend :: (SendFrom v) => Comm -> Rank -> v -> IO ()
gathervSend comm root segment = do
   -- myRank <- commRank comm
   -- XXX: assert myRank == root
   sendFrom segment $ \sendPtr sendElements sendType ->
     -- the recvPtr, counts and displacements are ignored in this case, so we can make it NULL
     Internal.gatherv (castPtr sendPtr) sendElements sendType nullPtr nullPtr nullPtr byte root comm

{- | A variation of 'gatherSend' and 'gatherRecv' where all members of
a group receive the result.

Caller is expected to make sure that types of send and receive buffers
are selected in a way such that amount of bytes sent equals amount of bytes received pairwise between all processes.
-}
allgather :: (SendFrom v1, RecvInto v2) => Comm -> v1 -> v2 -> IO ()
allgather comm sendVal recvVal = do
  sendFrom sendVal $ \sendPtr sendElements sendType ->
    recvInto recvVal $ \recvPtr _ _ -> -- Since amount sent equals amount received
      Internal.allgather (castPtr sendPtr) sendElements sendType (castPtr recvPtr) sendElements sendType comm

-- | A variation of 'allgather' that allows to use data segments of
--   different length.
allgatherv :: (SendFrom v1, RecvInto v2) => Comm
              -> v1 -- ^ Send buffer
              -> StorableArray Int CInt -- ^ Lengths of segments in the send buffer
              -> StorableArray Int CInt -- ^ Displacements of the segments in the send buffer
              -> v2 -- ^ Receive buffer
              -> IO ()
allgatherv comm segment counts displacements recvVal = do
   sendFrom segment $ \sendPtr sendElements sendType ->
     withStorableArray counts $ \countsPtr ->  
        withStorableArray displacements $ \displPtr -> 
          recvInto recvVal $ \recvPtr _ recvType ->
            Internal.allgatherv (castPtr sendPtr) sendElements sendType (castPtr recvPtr) countsPtr displPtr recvType comm
             
{- | Scatter/Gather data from all
members to all members of a group (also called complete exchange).

Caller is expected to make sure that types of send and receive buffers and send/receive counts
are selected in a way such that amount of bytes sent equals amount of bytes received pairwise between all processes.
-}
alltoall :: (SendFrom v1, RecvInto v2) => Comm 
            -> v1 -- ^ Send buffer
            -> Int -- ^ How many elements to /send/ to each process
            -> Int -- ^ How many elements to /receive/ from each process
            -> v2 -- ^ Receive buffer
            -> IO ()
alltoall comm sendVal sendCount recvCount recvVal =
  sendFrom sendVal $ \sendPtr _ sendType ->
    recvInto recvVal $ \recvPtr _ recvType -> -- Since amount sent must equal amount received
      Internal.alltoall (castPtr sendPtr) (cIntConv sendCount) sendType (castPtr recvPtr) (cIntConv recvCount) recvType comm

alltoallv :: (SendFrom v1, RecvInto v2) => Comm -> v1 -> StorableArray Int CInt -> StorableArray Int CInt -> StorableArray Int CInt -> StorableArray Int CInt -> v2 -> IO ()
alltoallv comm sendVal sendCounts sendDisplacements recvCounts recvDisplacements recvVal = do
  sendFrom sendVal $ \sendPtr _ sendType ->
    recvInto recvVal $ \recvPtr _ recvType ->
      withStorableArray sendCounts $ \sendCountsPtr ->
        withStorableArray sendDisplacements $ \sendDisplPtr ->
          withStorableArray recvCounts $ \recvCountsPtr ->
            withStorableArray recvDisplacements $ \recvDisplPtr ->
              Internal.alltoallv (castPtr sendPtr) sendCountsPtr sendDisplPtr sendType
                                 (castPtr recvPtr) recvCountsPtr recvDisplPtr recvType comm
  
reduceSend :: SendFrom v => Comm -> Rank -> Operation -> v -> IO ()
reduceSend comm root op sendVal = do
  sendFrom sendVal $ \sendPtr sendElements sendType ->
    Internal.reduce (castPtr sendPtr) nullPtr sendElements sendType op root comm

reduceRecv :: (SendFrom v, RecvInto v) => Comm -> Rank -> Operation -> v -> v -> IO ()
reduceRecv comm root op sendVal recvVal =
  sendFrom sendVal $ \sendPtr sendElements sendType ->
    recvInto recvVal $ \recvPtr _ _ ->
      Internal.reduce (castPtr sendPtr) (castPtr recvPtr) sendElements sendType op root comm

allreduce :: (SendFrom v, RecvInto v) => Comm -> Operation -> v -> v -> IO ()
allreduce comm op sendVal recvVal = 
  sendFrom sendVal $ \sendPtr sendElements sendType ->
    recvInto recvVal $ \recvPtr _ _ ->
      Internal.allreduce (castPtr sendPtr) (castPtr recvPtr) sendElements sendType op comm

reduceScatter :: (SendFrom v, RecvInto v) => Comm -> Operation -> StorableArray Int CInt -> v -> v -> IO ()
reduceScatter comm op counts sendVal recvVal =
  sendFrom sendVal $ \sendPtr _ sendType ->
    recvInto recvVal $ \recvPtr _ _ ->
      withStorableArray counts $ \countsPtr ->
      Internal.reduceScatter (castPtr sendPtr) (castPtr recvPtr) countsPtr sendType op comm

class Repr e where
  -- How many elements of given datatype do we need to represent given
  -- type in MPI transfers
  representation :: e -> (Int, Datatype)

-- | Representation is 'unsigned'
instance Repr Bool where
  representation _ = (1,unsigned)

-- | Note that C @int@ is alway 32-bit, while Haskell @Int@ size is platform-dependent. Therefore on 32-bit platforms 'int' 
-- is used to represent 'Int', and on 64-bit platforms 'longLong' is used
instance Repr Int where
#if SIZEOF_HSINT == 4  
  representation _ = (1,int)
#elif SIZEOF_HSINT == 8
  representation _ = (1,longLong)
#else
#error Haskell MPI bindings not tested on architecture where size of Haskell Int is not 4 or 8
#endif

-- | Representation is 'byte'
instance Repr Int8 where
   representation _ = (1,byte)
-- | Representation is 'short'
instance Repr Int16 where
   representation _ = (1,short)
-- | Representation is 'int'
instance Repr Int32 where
   representation _ = (1,int)
-- | Representation is 'longLong'
instance Repr Int64 where
   representation _ = (1,longLong)
-- | Representation is 'int'
instance Repr CInt where
  representation _ = (1,int)

-- | Representation is either 'int' or 'longLong', depending on the platform. See comments for @Repr Int@.
instance Repr Word where
#if SIZEOF_HSINT == 4  
  representation _ = (1,unsigned)
#else
  representation _ = (1,unsignedLongLong)
#endif

-- | Representation is 'byte'
instance Repr Word8 where
  representation _ = (1,byte)
-- | Representation is 'unsignedShort'
instance Repr Word16 where
  representation _ = (1,unsignedShort)
-- | Representation is 'unsigned'
instance Repr Word32 where
  representation _ = (1,unsigned)
-- | Representation is 'unsignedLongLong'
instance Repr Word64 where
  representation _ = (1,unsignedLongLong)

-- | Representation is 'wchar'
instance Repr Char where
  representation _ = (1,wchar)
-- | Representation is 'char'
instance Repr CChar where
  representation _ = (1,char)

-- | Representation is 'double'
instance Repr Double where
  representation _ = (1,double)
-- | Representation is 'float'
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

{- |
Binds a user-dened reduction operation to an 'Operation' handle that can
subsequently be used in 'reduceSend', 'reduceRecv', 'allreduce', and 'reduceScatter'.
The user-defined operation is assumed to be associative. 

If first argument to @opCreate@ is @True@, then the operation should be both commutative and associative. If
it is not commutative, then the order of operands is fixed and is defined to be in ascending,
process rank order, beginning with process zero. The order of evaluation can be changed,
taking advantage of the associativity of the operation. If operation
is commutative then the order
of evaluation can be changed, taking advantage of commutativity and
associativity.

User-defined operation accepts four arguments, @invec@, @inoutvec@,
@len@ and @datatype@ and applies reduction operation to the elements
of @invec@ and @inoutvec@ in pariwise manner. In pseudocode:

@
for i in [0..len-1] { inoutvec[i] = op invec[i] inoutvec[i] }
@

Full example with user-defined function that mimics standard operation
'sumOp':

@
import "Control.Parallel.MPI.Fast"

foreign import ccall \"wrapper\" 
  wrap :: (Ptr CDouble -> Ptr CDouble -> Ptr CInt -> Ptr Datatype -> IO ()) 
          -> IO (FunPtr (Ptr CDouble -> Ptr CDouble -> Ptr CInt -> Ptr Datatype -> IO ()))
reduceUserOpTest myRank = do
  numProcs <- commSize commWorld
  userSumPtr <- wrap userSum
  mySumOp <- opCreate True userSumPtr
  (src :: StorableArray Int Double) <- newListArray (0,99) [0..99]
  if myRank /= root
    then reduceSend commWorld root sumOp src
    else do
    (result :: StorableArray Int Double) <- intoNewArray_ (0,99) $ reduceRecv commWorld root mySumOp src
    recvMsg <- getElems result
  freeHaskellFunPtr userSumPtr
  where
    userSum :: Ptr CDouble -> Ptr CDouble -> Ptr CInt -> Ptr Datatype -> IO ()
    userSum inPtr inoutPtr lenPtr _ = do
      len <- peek lenPtr
      let offs = sizeOf ( undefined :: CDouble )
      let loop 0 _ _ = return ()
          loop n inPtr inoutPtr = do
            a <- peek inPtr
            b <- peek inoutPtr
            poke inoutPtr (a+b)
            loop (n-1) (plusPtr inPtr offs) (plusPtr inoutPtr offs)
      loop len inPtr inoutPtr
@
-}
opCreate :: Storable t => Bool
            -- ^ Whether the operation is commutative
            -> (FunPtr (Ptr t -> Ptr t -> Ptr CInt -> Ptr Datatype -> IO ())) 
            {- ^ Pointer to function that accepts, in order:
 
            * @invec@, pointer to first input vector

            * @inoutvec@, pointer to second input vector, which is also the output vector

            * @len@, pointer to length of both vectors

            * @datatype@, pointer to 'Datatype' of elements in both vectors
            -}
            -> IO Operation -- ^ Handle to the created user-defined operation
opCreate commute f = do
  Internal.opCreate (castFunPtr f) commute
