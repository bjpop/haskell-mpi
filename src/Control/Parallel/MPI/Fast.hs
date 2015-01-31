{-# LANGUAGE FlexibleContexts, FlexibleInstances, ScopedTypeVariables, UndecidableInstances, CPP #-}

-----------------------------------------------------------------------------
{- |
Module      : Control.Parallel.MPI.Fast
Copyright   : (c) 2010 Bernie Pope, Dmitry Astapov
License     : BSD-style
Maintainer  : florbitous@gmail.com
Stability   : experimental
Portability : ghc

This module provides the ability to transfer via MPI any Haskell value that could be
represented by some MPI type without expensive conversion or serialization.

Most of the \"primitive\" Haskell types could be treated this way, along with Storable and IO Arrays. Full range of point-to-point and collective operation is supported, including for reduce and similar operations.

Typeclass 'SendFrom' incapsulates the act of representing Haskell value as a flat memory region that could be used as a \"send buffer\" in MPI calls.

Likewise, 'RecvInto' captures the rules for using Haskell value as a \"receive buffer\" in MPI calls.

Correspondence between Haskell types and MPI types is encoded in 'Repr' typeclass.

Below is a small but complete MPI program utilising this Module. Process 0 sends the array of @Int@s
process 1. Process 1 receives the message and prints it
to standard output. It assumes that there are at least 2 MPI processes
available. Further examples in this module would provide different implementation of
@process@ function.

@
\{\-\# LANGUAGE ScopedTypeVariables \#\-\}

module Main where

import Control.Parallel.MPI.Fast
import Data.Array.Storable

type ArrMsg = StorableArray Int Int

bounds :: (Int, Int)
bounds = (1,10)

arrMsg :: IO (StorableArray Int Int)
arrMsg = newListArray bounds [1..10]

main :: IO ()
main = mpi $ do
   rank <- commRank commWorld
   process rank

process :: Rank -> IO ()
process rank
   | rank == 0 = do sendMsg <- arrMsg
                    send commWorld 1 2 sendMsg
   | rank == 1 = do (recvMsg::ArrMsg, status) <- intoNewArray bounds $ recv commWorld 0 2
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
   , ssend
   , rsend
   , recv
     -- ** Non-blocking.
   , isend
   , issend
   , irecv
   , isendPtr
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
   , reduceScatterBlock
   , reduceScatter
   , opCreate
   , Internal.opFree

   , module Data.Word
   , module Control.Parallel.MPI.Base
   ) where

#include "MachDeps.h"

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
import Foreign
import Foreign.C.Types

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


{- | Helper wrapper function that would allocate array of the given size and use it as receive buffer, without the need to
preallocate it explicitly.

Most of the functions in this API could reuse receive buffer (like 'StorableArray') over and over again.
If you do not have preallocated buffer you could use this wrapper to get yourself one.

Consider the following code that uses preallocated buffer:

@
scattervRecv root comm arr
@

Same code with buffer allocation:

@
(arr,status) <- intoNewArray range $ scattervRecv root comm
@
-}
intoNewArray :: (Ix i, MArray a e m, RecvInto (a i e)) => (i, i) -> (a i e -> m r) -> m (a i e, r)
intoNewArray range f = do
  arr <- unsafeNewArray_ range -- New, uninitialized array, According to http://hackage.haskell.org/trac/ghc/ticket/3586
                               -- should be faster than newArray_
  res <- f arr
  return (arr, res)

-- | Variant of 'intoNewArray' that discards the result of the wrapped function.
-- Useful for discarding @()@ from functions like 'scatterSend' that return @IO ()@
intoNewArray_ :: (Ix i, MArray a e m, RecvInto (a i e)) => (i, i) -> (a i e -> m r) -> m (a i e)
intoNewArray_ range f = do
  arr <- unsafeNewArray_ range
  _ <- f arr
  return arr

-- | Sends @v@ to the process identified by @(Comm, Rank, Tag)@. Call will return as soon as MPI has copied data from its internal send buffer.
send :: (SendFrom v) => Comm -> Rank -> Tag -> v -> IO ()
send  = sendWith Internal.send

-- | Sends @v@ to the process identified by @(Comm, Rank, Tag)@. Call will return as soon as receiving process started receiving data.
ssend :: (SendFrom v) => Comm -> Rank -> Tag -> v -> IO ()
ssend = sendWith Internal.ssend

-- | Sends @v@ to the process identified by @(Comm, Rank, Tag)@. Matching 'recv' should already be posted, otherwise MPI error could occur.
rsend :: (SendFrom v) => Comm -> Rank -> Tag -> v -> IO ()
rsend = sendWith Internal.rsend

type SendPrim = Ptr () -> CInt -> Datatype -> Rank -> Tag -> Comm -> IO ()

sendWith :: (SendFrom v) => SendPrim -> Comm -> Rank -> Tag -> v -> IO ()
sendWith send_function comm rank tag val = do
   sendFrom val $ \valPtr numBytes dtype -> do
      send_function (castPtr valPtr) numBytes dtype rank tag comm

-- | Receives data from the process identified by @(Comm, Rank, Tag)@ and store it in @v@.
recv :: (RecvInto v) => Comm -> Rank -> Tag -> v -> IO Status
recv comm rank tag arr = do
   recvInto arr $ \valPtr numBytes dtype ->
      Internal.recv (castPtr valPtr) numBytes dtype rank tag comm

-- | \"Root\" process identified by @(Comm, Rank)@ sends value of @v@ to all processes in communicator @Comm@.
bcastSend :: (SendFrom v) => Comm -> Rank -> v -> IO ()
bcastSend comm sendRank val = do
   sendFrom val $ \valPtr numBytes dtype -> do
      Internal.bcast (castPtr valPtr) numBytes dtype sendRank comm

-- | Receive data distributed via 'bcaseSend' and store it in @v@.
bcastRecv :: (RecvInto v) => Comm -> Rank -> v -> IO ()
bcastRecv comm sendRank val = do
   recvInto val $ \valPtr numBytes dtype -> do
      Internal.bcast (castPtr valPtr) numBytes dtype sendRank comm

-- | Sends @v@ to the process identified by @(Comm, Rank, Tag)@ in non-blocking mode. @Request@ will be considered complete as soon as MPI copies the data from the send buffer. Use 'probe', 'test', 'cancel' or 'wait' to work with @Request@.
isend :: (SendFrom v) => Comm -> Rank -> Tag -> v -> IO Request
isend  = isendWith Internal.isend

-- | Sends @v@ to the process identified by @(Comm, Rank, Tag)@ in non-blocking mode. @Request@ will be considered complete as soon as receiving process starts to receive data.
issend :: (SendFrom v) => Comm -> Rank -> Tag -> v -> IO Request
issend = isendWith Internal.issend

type ISendPrim = Ptr () -> CInt -> Datatype -> Rank -> Tag -> Comm -> IO (Request)

isendWith :: (SendFrom v) => ISendPrim -> Comm -> Rank -> Tag -> v -> IO Request
isendWith send_function comm recvRank tag val = do
  sendFrom val $ \valPtr numBytes dtype -> do
    send_function valPtr numBytes dtype recvRank tag comm

-- | Variant of 'isend' that stores @Request@ at the provided pointer. Useful for filling up arrays of @Request@s that would later be fed to 'waitall'.
isendPtr :: (SendFrom v) => Comm -> Rank -> Tag -> Ptr Request -> v -> IO ()
isendPtr  = isendWithPtr Internal.isendPtr

-- | Variant of 'issend' that stores @Request@ at the provided pointer. Useful for filling up arrays of @Request@s that would later be fed to 'waitall'.
issendPtr :: (SendFrom v) => Comm -> Rank -> Tag -> Ptr Request -> v -> IO ()
issendPtr = isendWithPtr Internal.issendPtr

type ISendPtrPrim = Ptr () -> CInt -> Datatype -> Rank -> Tag -> Comm -> Ptr Request -> IO ()
isendWithPtr :: (SendFrom v) => ISendPtrPrim -> Comm -> Rank -> Tag -> Ptr Request -> v -> IO ()
isendWithPtr send_function comm recvRank tag requestPtr val = do
   sendFrom val $ \valPtr numBytes dtype ->
     send_function (castPtr valPtr) numBytes dtype recvRank tag comm requestPtr

-- | Variant of 'irecv' that stores @Request@ at the provided pointer.
irecvPtr :: (Storable e, Ix i, Repr e) => Comm -> Rank -> Tag -> Ptr Request -> StorableArray i e -> IO ()
irecvPtr comm sendRank tag requestPtr recvVal = do
  recvInto recvVal $ \recvPtr recvElements recvType -> do
    Internal.irecvPtr (castPtr recvPtr) recvElements recvType sendRank tag comm requestPtr

{-| Receive 'StorableArray' from the process identified by @(Comm, Rank, Tag)@ in non-blocking mode.

At the moment we are limiting this to 'StorableArray's because they
are compatible with C pointers. This means that the recieved data can
be written directly to the array, and does not have to be copied out
at the end. This is important for the non-blocking operation of @irecv@.

It is not safe to copy the data from the C pointer until the transfer
is complete. So any array type which requires copying of data after
receipt of the message would have to wait on complete transmission.
It is not clear how to incorporate the waiting automatically into
the same interface as the one below. One option is to use a Haskell
thread to do the data copying in the \"background\" (as was done for 'Simple.irecv'). Another option
is to introduce a new kind of data handle which would encapsulate the
wait operation, and would allow the user to request the data to be
copied when the wait was complete.
-}
irecv :: (Storable e, Ix i, Repr e) => Comm -> Rank -> Tag -> StorableArray i e -> IO Request
irecv comm sendRank tag recvVal = do
   recvInto recvVal $ \recvPtr recvElements recvType -> do
      Internal.irecv (castPtr recvPtr) recvElements recvType sendRank tag comm

-- | Wrapper around 'Internal.waitall' that operates on 'StorableArray's
waitall :: StorableArray Int Request -> StorableArray Int Status -> IO ()
waitall requests statuses = do
  cnt <- rangeSize <$> getBounds requests
  withStorableArray requests $ \reqs ->
    withStorableArray statuses $ \stats ->
      Internal.waitall (fromIntegral cnt) (castPtr reqs) (castPtr stats)

-- | Scatter elements of @v1@ to all members of communicator @Comm@ from the \"root\" process identified by @Rank@. Receive own slice of data
-- in @v2@. Note that when @Comm@ is inter-communicator, @Rank@ could differ from the rank of the calling process.
scatterSend :: (SendFrom v1, RecvInto v2) => Comm -> Rank -> v1 -> v2 -> IO ()
scatterSend comm root sendVal recvVal = do
   recvInto recvVal $ \recvPtr recvElements recvType ->
     sendFrom sendVal $ \sendPtr _ _ ->
       Internal.scatter (castPtr sendPtr) recvElements recvType (castPtr recvPtr) recvElements recvType root comm

-- | Receive the slice of data scattered from \"root\" process identified by @(Comm, Rank)@ and store it into @v@.
scatterRecv :: (RecvInto v) => Comm -> Rank -> v -> IO ()
scatterRecv comm root recvVal = do
   recvInto recvVal $ \recvPtr recvElements recvType ->
     Internal.scatter nullPtr 0 byte (castPtr recvPtr) recvElements recvType root comm

-- | Variant of 'scatterSend' that allows to send data in uneven chunks.
-- Since interface is tailored for speed, @counts@ and @displacements@ should be in 'StorableArray's.
scattervSend :: (SendFrom v1, RecvInto v2) => Comm
                -> Rank
                -> v1 -- ^ Value (vector) to send from
                -> StorableArray Int CInt -- ^ Length of each segment (in elements)
                -> StorableArray Int CInt -- ^ Offset of each segment from the beginning of @v1@ (in elements)
                -> v2
                -> IO ()
scattervSend comm root sendVal counts displacements recvVal  = do
   -- myRank <- commRank comm
   -- XXX: assert myRank == sendRank ?
   recvInto recvVal $ \recvPtr recvElements recvType ->
     sendFrom sendVal $ \sendPtr _ sendType->
       withStorableArray counts $ \countsPtr ->
         withStorableArray displacements $ \displPtr ->
           Internal.scatterv (castPtr sendPtr) countsPtr displPtr sendType
                             (castPtr recvPtr) recvElements recvType root comm

-- | Variant of 'scatterRecv', to be used with 'scattervSend'
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
-- | \"Root\" process identified by @(Comm, Rank)@ collects data sent via 'gatherSend' and stores them in @v2@. Collecting process supplies
-- its own share of data in @v1@.
gatherRecv :: (SendFrom v1, RecvInto v2) => Comm -> Rank -> v1 -> v2 -> IO ()
gatherRecv comm root segment recvVal = do
   -- myRank <- commRank comm
   -- XXX: assert myRank == root
   sendFrom segment $ \sendPtr sendElements sendType ->
     recvInto recvVal $ \recvPtr _ _ ->
       Internal.gather (castPtr sendPtr) sendElements sendType (castPtr recvPtr) sendElements sendType root comm

-- | Send value of @v@ to the \"root\" process identified by @(Comm, Rank)@, to be collected with 'gatherRecv'.
gatherSend :: (SendFrom v) => Comm -> Rank -> v -> IO ()
gatherSend comm root segment = do
   -- myRank <- commRank comm
   -- XXX: assert it is /= root
   sendFrom segment $ \sendPtr sendElements sendType ->
     -- the recvPtr is ignored in this case, so we can make it NULL, likewise recvCount can be 0
     Internal.gather (castPtr sendPtr) sendElements sendType nullPtr 0 byte root comm

-- | Variant of 'gatherRecv' that allows to collect data segments of uneven size (see 'scattervSend' for details)
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

-- | Variant of 'gatherSend', to be used with 'gathervRecv'.
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
      Internal.alltoall (castPtr sendPtr) (fromIntegral sendCount) sendType (castPtr recvPtr) (fromIntegral recvCount) recvType comm

-- | A variation of 'alltoall' that allows to use data segments of
--   different length.
alltoallv :: (SendFrom v1, RecvInto v2) => Comm
             -> v1 -- ^ Send buffer
             -> StorableArray Int CInt -- ^ Lengths of segments in the send buffer
             -> StorableArray Int CInt -- ^ Displacements of the segments in the send buffer
             -> StorableArray Int CInt -- ^ Lengths of segments in the receive buffer
             -> StorableArray Int CInt -- ^ Displacements of the segments in the receive buffer
             -> v2 -- ^ Receive buffer
             -> IO ()
alltoallv comm sendVal sendCounts sendDisplacements recvCounts recvDisplacements recvVal = do
  sendFrom sendVal $ \sendPtr _ sendType ->
    recvInto recvVal $ \recvPtr _ recvType ->
      withStorableArray sendCounts $ \sendCountsPtr ->
        withStorableArray sendDisplacements $ \sendDisplPtr ->
          withStorableArray recvCounts $ \recvCountsPtr ->
            withStorableArray recvDisplacements $ \recvDisplPtr ->
              Internal.alltoallv (castPtr sendPtr) sendCountsPtr sendDisplPtr sendType
                                 (castPtr recvPtr) recvCountsPtr recvDisplPtr recvType comm

{-| Reduce values from a group of processes into single value, which is delivered to single (so-called root) process.
See 'reduceRecv' for function that should be called by root process.

If the value is scalar, then reduction is similar to 'fold1'. For example, if the opreration is 'sumOp', then
@reduceSend@ would compute sum of values supplied by all processes.
-}
reduceSend :: SendFrom v => Comm
              -> Rank -- ^ Rank of the root process
              -> Operation -- ^ Reduction operation
              -> v -- ^ Value supplied by this process
              -> IO ()
reduceSend comm root op sendVal = do
  sendFrom sendVal $ \sendPtr sendElements sendType ->
    Internal.reduce (castPtr sendPtr) nullPtr sendElements sendType op root comm

{-| Obtain result of reduction initiated by 'reduceSend'. Note that root process supplies value for reduction as well.
-}
reduceRecv :: (SendFrom v, RecvInto v) => Comm
              -> Rank -- ^ Rank of the root process
              -> Operation  -- ^ Reduction operation
              -> v -- ^ Value supplied by this process
              -> v -- ^ Reduction result
              -> IO ()
reduceRecv comm root op sendVal recvVal =
  sendFrom sendVal $ \sendPtr sendElements sendType ->
    recvInto recvVal $ \recvPtr _ _ ->
      Internal.reduce (castPtr sendPtr) (castPtr recvPtr) sendElements sendType op root comm

-- | Variant of 'reduceSend' and 'reduceRecv', where result is delivered to all participating processes.
allreduce :: (SendFrom v, RecvInto v) =>
             Comm -- ^ Communicator engaged in reduction/
             -> Operation -- ^ Reduction operation
             -> v -- ^ Value supplied by this process
             -> v -- ^ Reduction result
             -> IO ()
allreduce comm op sendVal recvVal =
  sendFrom sendVal $ \sendPtr sendElements sendType ->
    recvInto recvVal $ \recvPtr _ _ ->
      Internal.allreduce (castPtr sendPtr) (castPtr recvPtr) sendElements sendType op comm

-- | Combination of 'reduceSend' + 'reduceRecv' and 'scatterSend' + 'scatterRecv': reduction result
-- is split and scattered among participating processes.
--
-- See 'reduceScatter' if you want to be able to specify personal block size for each process.
--
-- Note that this function is not supported with OpenMPI 1.5
reduceScatterBlock :: (SendFrom v, RecvInto v) =>
                 Comm -- ^ Communicator engaged in reduction/
                 -> Operation -- ^ Reduction operation
                 -> Int -- ^ Size of the result block sent to each process
                 -> v -- ^ Value supplied by this process
                 -> v -- ^ Reduction result
                 -> IO ()
reduceScatterBlock comm op blocksize sendVal recvVal =
  sendFrom sendVal $ \sendPtr _ sendType ->
    recvInto recvVal $ \recvPtr _ _ ->
      Internal.reduceScatterBlock (castPtr sendPtr) (castPtr recvPtr) (fromIntegral blocksize :: CInt) sendType op comm

-- | Combination of 'reduceSend' / 'reduceRecv' and 'scatterSend' / 'scatterRecv': reduction result
-- is split and scattered among participating processes.
reduceScatter :: (SendFrom v, RecvInto v) =>
                 Comm -- ^ Communicator engaged in reduction/
                 -> Operation -- ^ Reduction operation
                 -> StorableArray Int CInt -- ^ Sizes of block distributed to each process
                 -> v -- ^ Value supplied by this process
                 -> v -- ^ Reduction result
                 -> IO ()
reduceScatter comm op counts sendVal recvVal =
  sendFrom sendVal $ \sendPtr _ sendType ->
    recvInto recvVal $ \recvPtr _ _ ->
      withStorableArray counts $ \countsPtr ->
      Internal.reduceScatter (castPtr sendPtr) (castPtr recvPtr) countsPtr sendType op comm

-- |  How many (consecutive) elements of given datatype do we need to represent given
--   the Haskell type in MPI operations
class Repr e where
  representation :: e -> (Int, Datatype)

-- | Representation is one 'unsigned'
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

-- | Representation is one 'byte'
instance Repr Int8 where
   representation _ = (1,byte)
-- | Representation is one 'short'
instance Repr Int16 where
   representation _ = (1,short)
-- | Representation is one 'int'
instance Repr Int32 where
   representation _ = (1,int)
-- | Representation is one 'longLong'
instance Repr Int64 where
   representation _ = (1,longLong)
-- | Representation is one 'int'
instance Repr CInt where
  representation _ = (1,int)

-- | Representation is either one 'int' or one 'longLong', depending on the platform. See comments for @Repr Int@.
instance Repr Word where
#if SIZEOF_HSINT == 4
  representation _ = (1,unsigned)
#else
  representation _ = (1,unsignedLongLong)
#endif

-- | Representation is one 'byte'
instance Repr Word8 where
  representation _ = (1,byte)
-- | Representation is one 'unsignedShort'
instance Repr Word16 where
  representation _ = (1,unsignedShort)
-- | Representation is one 'unsigned'
instance Repr Word32 where
  representation _ = (1,unsigned)
-- | Representation is one 'unsignedLongLong'
instance Repr Word64 where
  representation _ = (1,unsignedLongLong)

-- | Representation is one 'wchar'
instance Repr Char where
  representation _ = (1,wchar)
-- | Representation is one 'char'
instance Repr CChar where
  representation _ = (1,char)

-- | Representation is one 'double'
instance Repr Double where
  representation _ = (1,double)
-- | Representation is one 'float'
instance Repr Float where
  representation _ = (1,float)

instance Repr e => Repr (StorableArray i e) where
  representation _ = representation (undefined::e)

instance Repr e => Repr (IOArray i e) where
  representation _ = representation (undefined::e)

instance Repr e => Repr (IOUArray i e) where
  representation _ = representation (undefined::e)

{- | Treat @v@ as send buffer suitable for the purposes of this API.

Method 'sendFrom' is expected to deduce how to use @v@ as a memory-mapped buffer that consist of a number of
elements of some 'Datatype'. It would then call the supplied function, passing it the pointer to the buffer,
its size (in elements) and type of the element.

Note that @e@ is not bound by the typeclass, so all kinds of foul play
are possible. However, since MPI declares all buffers as @void*@ anyway,
we are not making life all /that/ unsafe with this.
-}
class SendFrom v where
   sendFrom :: v -- ^ Value to use as send buffer
               -> (Ptr e -> CInt -> Datatype -> IO a) -- ^ Function that will accept pointer to buffer, its length and type of buffer elements
               -> IO a

{- | Treat @v@ as receive buffer for the purposes of this API.
-}
class RecvInto v where
   recvInto :: v -- ^ Value to use as receive buffer
               -> (Ptr e -> CInt -> Datatype -> IO a)  -- ^ Function that will accept pointer to buffer, its length and type of buffer elements
               -> IO a

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

-- | Sending from Storable arrays requres knowing MPI representation 'Repr' of its elements. This is very
-- fast and efficient, since array would be updated in-place.
instance (Storable e, Repr e, Ix i) => SendFrom (StorableArray i e) where
  sendFrom = withStorableArrayAndSize

-- | Receiving into Storable arrays requres knowing MPI representation 'Repr' of its elements. This is very
-- fast and efficient, since array would be updated in-place.
instance (Storable e, Repr e, Ix i) => RecvInto (StorableArray i e) where
  recvInto = withStorableArrayAndSize

withStorableArrayAndSize :: forall a i e z.(Repr e, Storable e, Ix i) => StorableArray i e -> (Ptr z -> CInt -> Datatype -> IO a) -> IO a
withStorableArrayAndSize arr f = do
   rSize <- rangeSize <$> getBounds arr
   let (scale, dtype) = (representation (undefined :: StorableArray i e))
       numElements = fromIntegral (rSize * scale)
   withStorableArray arr $ \ptr -> f (castPtr ptr) numElements dtype

-- | This is less efficient than using 'StorableArray'
-- since extra memory copy is required to represent array as continuous memory buffer.
instance (Storable e, Repr (IOArray i e), Ix i) => SendFrom (IOArray i e) where
  sendFrom = sendWithMArrayAndSize
-- | This is less efficient than using 'StorableArray'
-- since extra memory copy is required to construct the resulting array.
instance (Storable e, Repr (IOArray i e), Ix i) => RecvInto (IOArray i e) where
  recvInto = recvWithMArrayAndSize

recvWithMArrayAndSize :: forall i e r a z. (Storable e, Ix i, MArray a e IO, Repr (a i e)) => a i e -> (Ptr z -> CInt -> Datatype -> IO r) -> IO r
recvWithMArrayAndSize array f = do
   bounds <- getBounds array
   let (scale, dtype) = representation (undefined :: a i e)
       numElements = fromIntegral $ rangeSize bounds * scale
   allocaArray (rangeSize bounds) $ \ptr -> do
      result <- f (castPtr ptr) numElements dtype
      fillArrayFromPtr (range bounds) (rangeSize bounds) ptr array
      return result

sendWithMArrayAndSize :: forall i e r a z. (Storable e, Ix i, MArray a e IO, Repr (a i e)) => a i e -> (Ptr z -> CInt -> Datatype -> IO r) -> IO r
sendWithMArrayAndSize array f = do
   elements <- getElems array
   bounds <- getBounds array
   let (scale, dtype) = representation (undefined :: a i e)
       numElements = fromIntegral $ rangeSize bounds * scale
   withArray elements $ \ptr -> f (castPtr ptr) numElements dtype

-- XXX I wonder if this can be written without the intermediate list?
-- Maybe GHC can elimiate it. We should look at the generated compiled
-- code to see how well the loop is handled.
fillArrayFromPtr :: (MArray a e IO, Storable e, Ix i) => [i] -> Int -> Ptr e -> a i e -> IO ()
fillArrayFromPtr indices numElements startPtr array = do
   elems <- peekArray numElements startPtr
   mapM_ (\(index, element) -> writeArray array index element ) (zip indices elems)

-- | Sending from ByteString is efficient, since it already has the necessary memory layout.
instance SendFrom BS.ByteString where
  sendFrom = sendWithByteStringAndSize

sendWithByteStringAndSize :: BS.ByteString -> (Ptr z -> CInt -> Datatype -> IO a) -> IO a
sendWithByteStringAndSize bs f = do
  unsafeUseAsCStringLen bs $ \(bsPtr,len) -> f (castPtr bsPtr) (fromIntegral len) byte

-- | Receiving into pointers to 'Storable' scalars with known MPI representation
instance (Storable e, Repr e) => RecvInto (Ptr e) where
  recvInto = recvIntoElemPtr (representation (undefined :: e))
    where
      recvIntoElemPtr (cnt,datatype) p f = f (castPtr p) (fromIntegral cnt) datatype

-- | Receiving into pointers to 'Storable' vectors with known MPI representation and length
instance (Storable e, Repr e) => RecvInto (Ptr e, Int) where
  recvInto = recvIntoVectorPtr (representation (undefined :: e))
    where
      recvIntoVectorPtr (scale, datatype) (p,len) f = f (castPtr p) (fromIntegral (len * scale) :: CInt) datatype

-- | Allocate new 'Storable' value and use it as receive buffer
intoNewVal :: (Storable e) => (Ptr e -> IO r) -> IO (e, r)
intoNewVal f = do
  alloca $ \ptr -> do
    res <- f ptr
    val <- peek ptr
    return (val, res)

-- | Variant of 'intoNewVal' that discards result of the wrapped function
intoNewVal_ :: (Storable e) => (Ptr e -> IO r) -> IO e
intoNewVal_ f = do
  (val, _) <- intoNewVal f
  return val

-- | Allocate new 'ByteString' of the given length and use it as receive buffer
intoNewBS :: Integral a => a -> ((Ptr CChar,Int) -> IO r) -> IO (BS.ByteString, r)
intoNewBS len f = do
  let l = fromIntegral len
  allocaBytes l $ \ptr -> do
    res <- f (ptr, l)
    bs <- BS.packCStringLen (ptr, l)
    return (bs, res)

-- | Variant of 'intoNewBS' that discards result of the wrapped function
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
