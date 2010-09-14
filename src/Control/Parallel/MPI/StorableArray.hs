{-# LANGUAGE FlexibleContexts, ScopedTypeVariables #-}

module Control.Parallel.MPI.StorableArray
   ( send
   , ssend
   , bsend
   , rsend
   , recv
   , bcast
   , isend
   , ibsend
   , issend
   , irecv
   , scatter
   , gather
   ) where

import C2HS
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
import Control.Parallel.MPI.Common (commRank)

send, bsend, ssend, rsend :: forall e i. (Storable e, Ix i) => StorableArray i e -> Rank -> Tag -> Comm -> IO ()
send  = sendWith Internal.send
bsend = sendWith Internal.bsend
ssend = sendWith Internal.ssend
rsend = sendWith Internal.rsend

sendWith :: forall e i. (Storable e, Ix i) =>
  (Ptr () -> CInt -> Datatype -> CInt -> CInt -> Comm -> IO CInt) ->
  StorableArray i e -> Rank -> Tag -> Comm -> IO ()
sendWith send_function array rank tag comm = do
   bounds <- getBounds array
   let arraySize = rangeSize bounds
       cRank = fromRank rank
       cTag  = fromTag tag
       elementSize = sizeOf (undefined :: e)
       numBytes = cIntConv (arraySize * elementSize)
   withStorableArray array $ \arrayPtr -> do
      checkError $ send_function (castPtr arrayPtr) numBytes byte cRank cTag comm

recv :: forall e i . (Storable e, Ix i) => (i,i) -> Rank -> Tag -> Comm -> IO (Status, StorableArray i e)
recv range rank tag comm = do
   let cRank = fromRank rank
       cTag = fromTag tag
       elementSize = sizeOf (undefined :: e)
       numElements = rangeSize range
       cBytes = cIntConv (numElements * elementSize)
   foreignPtr <- mallocForeignPtrArray numElements
   withForeignPtr foreignPtr $ \arrayPtr ->
      alloca $ \statusPtr -> do
         checkError $ Internal.recv (castPtr arrayPtr) cBytes byte cRank cTag comm (castPtr statusPtr)
         recvStatus <- peek statusPtr
         storableArray <- unsafeForeignPtrToStorableArray foreignPtr range
         return (recvStatus, storableArray)

bcast :: forall e i. (Storable e, Ix i) => StorableArray i e -> (i, i) -> Rank -> Comm -> IO (StorableArray i e)
bcast array range sendRank comm = do
   myRank <- commRank comm
   let cRank = fromRank sendRank
       elementSize = sizeOf (undefined :: e)
       numElements = rangeSize range
       cBytes = cIntConv (numElements * elementSize)
   if myRank == sendRank
      then withStorableArray array $ \arrayPtr -> do
              checkError $ Internal.bcast (castPtr arrayPtr) cBytes byte cRank comm
              return array
      else do
         foreignPtr <- mallocForeignPtrArray numElements
         withForeignPtr foreignPtr $ \arrayPtr -> do
            checkError $ Internal.bcast (castPtr arrayPtr) cBytes byte cRank comm
            unsafeForeignPtrToStorableArray foreignPtr range

isend, ibsend, issend :: forall e i . (Storable e, Ix i) => StorableArray i e -> Rank -> Tag -> Comm -> IO Request
isend  = isendWith Internal.isend
ibsend = isendWith Internal.ibsend
issend = isendWith Internal.issend

isendWith :: forall e i . (Storable e, Ix i) =>
  (Ptr () -> CInt -> Datatype -> CInt -> CInt -> Comm -> Ptr (Request) -> IO CInt) ->
  StorableArray i e -> Rank -> Tag -> Comm -> IO Request
isendWith send_function array recvRank tag comm = do
   bounds <- getBounds array
   let arraySize = rangeSize bounds
       cRank = fromRank recvRank
       cTag  = fromTag tag
       elementSize = sizeOf (undefined :: e)
       cBytes = cIntConv (arraySize * elementSize)
   alloca $ \requestPtr ->
      withStorableArray array $ \arrayPtr -> do
         checkError $ send_function (castPtr arrayPtr) cBytes byte cRank cTag comm requestPtr
         peek requestPtr

irecv :: forall e i . (Storable e, Ix i) => (i, i) -> Rank -> Tag -> Comm -> IO (StorableArray i e, Request)
irecv range sendRank tag comm = do
   let cRank = fromRank sendRank
       cTag  = fromTag tag
       elementSize = sizeOf (undefined :: e)
       numElements = rangeSize range
       cBytes = cIntConv (numElements * elementSize)
   alloca $ \requestPtr -> do
      foreignPtr <- mallocForeignPtrArray numElements
      withForeignPtr foreignPtr $ \arrayPtr -> do
         checkError $ Internal.irecv (castPtr arrayPtr) cBytes byte cRank cTag comm requestPtr
         array <- unsafeForeignPtrToStorableArray foreignPtr range
         request <- peek requestPtr
         return (array, request)
{-
   MPI_Scatter allows the element types of the send and recv buffers to be different.
   This is accommodated by changing the sendcount and recvcount arguments. I'm not sure
   about the utility of that feature in C, and I'm even less sure it would be a good idea
   in a strongly typed language like Haskell. So this version of scatter requires that
   the send and recv element types (e) are the same. Note we also use a (i,i) range
   argument instead of a sendcount argument. This makes it easier to work with
   arbitrary Ix types, instead of just integers. This gives scatter the same type signature
   as bcast, although the role of the range is different in the two. In bcast the range
   specifies the lower and upper indices of the entire sent/recv array. In scatter
   the range specifies the lower and upper indices of the sent/recv segmen of the
   array.

   XXX is it an error if:
      (arraySize `div` segmentSize) /= commSize
-}
scatter :: forall e i. (Storable e, Ix i) => StorableArray i e -> (i, i) -> Rank -> Comm -> IO (StorableArray i e)
scatter array range root comm = do
   let cRank = fromRank root
       elementSize = sizeOf (undefined :: e)
       numElements = rangeSize range
       numBytes = cIntConv (numElements * elementSize)
   myRank <- commRank comm
   if myRank == root
      then withStorableArray array $ \sendPtr -> do
         foreignPtr <- mallocForeignPtrArray numElements
         withForeignPtr foreignPtr $ \recvPtr -> do
            checkError $ Internal.scatter (castPtr sendPtr) numBytes byte (castPtr recvPtr) numBytes byte cRank comm
            unsafeForeignPtrToStorableArray foreignPtr range
      else do
         foreignPtr <- mallocForeignPtrArray numElements
         withForeignPtr foreignPtr $ \recvPtr -> do
            -- the sendPtr is ignored in this case, so we can make it NULL.
            checkError $ Internal.scatter nullPtr numBytes byte (castPtr recvPtr) numBytes byte cRank comm
            unsafeForeignPtrToStorableArray foreignPtr range

{-
Note the slightly odd semantics that on non-root processes the result array is the same as
the input array. You might think it would be sensible to return an empty array - but we can't
actually construct one, because we've no way of making empty bounds. It might also be
tempting to use a Maybe type to wrap the result, but this would ultimately just be
frustrating for the root process, who would always get their result in a Just
constructor.

XXX we should check that the outRange is large enough to store:

   segmentSize * commSize
-}

gather :: forall e i. (Storable e, Ix i) => StorableArray i e -> (i, i) -> Rank -> Comm -> IO (StorableArray i e)
gather segment outRange root comm = do
   segmentSize <- rangeSize <$> getBounds segment
   let cRank = fromRank root
       elementSize = sizeOf (undefined :: e)
       segmentBytes = cIntConv (segmentSize * elementSize)
   myRank <- commRank comm
   if myRank == root
      then do
         let numElements = rangeSize outRange
         foreignPtr <- mallocForeignPtrArray numElements
         withStorableArray segment $ \sendPtr ->
            withForeignPtr foreignPtr $ \recvPtr -> do
               checkError $ Internal.gather (castPtr sendPtr) segmentBytes byte (castPtr recvPtr) segmentBytes byte cRank comm
               unsafeForeignPtrToStorableArray foreignPtr outRange
      else
         withStorableArray segment $ \sendPtr -> do
            -- the recvPtr is ignored in this case, so we can make it NULL, likewise recvCount can be 0
            checkError $ Internal.gather (castPtr sendPtr) segmentBytes byte nullPtr 0 byte cRank comm
            return segment
