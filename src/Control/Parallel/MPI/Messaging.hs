{-# LANGUAGE TypeFamilies, ScopedTypeVariables, FlexibleInstances, FlexibleContexts #-}

module Control.Parallel.MPI.Messaging
   ( SendFrom (..)
   , RecvToMut (..)
   , RecvToImmut (..)
   , send
   , ssend
   , bsend
   , rsend
--   , recv
   , recvMut
   , recvImmut
   , bcastSend
   , bcastRecvMut
   , bcastRecvImmut
   , isend
   , ibsend
   , issend
   -- , irecv
   , sendScatter
   , recvScatterMut
--   , sendScatterv
--   , recvScatterv
   , sendGather
   , recvGatherMut
--   , sendGatherv
--   , recvGatherv
   , withNewArray_
   , withNewArray
   ) where

import C2HS
import qualified Control.Parallel.MPI.Internal as Internal
import Data.Array.Storable
import Data.ByteString.Unsafe (unsafeUseAsCString)
import Data.ByteString (ByteString, packCStringLen)
import qualified Data.ByteString as BS (length)
import Data.Array.IO
import Data.Array.Base (unsafeNewArray_)
import Data.Word
import Control.Applicative ((<$>))
import Control.Parallel.MPI.Datatype as Datatype
import Control.Parallel.MPI.Comm as Comm
import Control.Parallel.MPI.Status as Status
import Control.Parallel.MPI.Utils (checkError)
import Control.Parallel.MPI.Tag as Tag
import Control.Parallel.MPI.Rank as Rank
import Control.Parallel.MPI.Request as Request

class SendFrom a where
   sendFrom :: a -> (Ptr b -> CInt -> IO c) -> IO c

class RecvToMut a where
   recvToMut :: a -> (Ptr b -> CInt -> IO c) -> IO c

class RecvToImmut a where
   type Size a :: *
   recvToImmut :: Size a -> (Ptr b -> CInt -> IO c) -> IO (a, c)

send, bsend, ssend, rsend :: SendFrom a => Comm -> Rank -> Tag -> a -> IO ()
send  = sendWith Internal.send
bsend = sendWith Internal.bsend
ssend = sendWith Internal.ssend
rsend = sendWith Internal.rsend

type SendPrim = Ptr () -> CInt -> Datatype -> CInt -> CInt -> Comm -> IO CInt

sendWith :: SendFrom a => SendPrim -> Comm -> Rank -> Tag -> a -> IO ()
sendWith send_function comm rank tag msg = do
   sendFrom msg $ \arrayPtr numBytes -> do
      checkError $ send_function (castPtr arrayPtr) numBytes byte (fromRank rank) (fromTag tag) comm

recvMut :: RecvToMut a => Comm -> Rank -> Tag -> a -> IO Status
recvMut comm rank tag receptacle = do
   recvToMut receptacle $ \arrayPtr numBytes ->
      alloca $ \statusPtr -> do
         checkError $ Internal.recv (castPtr arrayPtr) numBytes byte (fromRank rank) (fromTag tag) comm (castPtr statusPtr)
         peek statusPtr

recvImmut :: RecvToImmut a => Comm -> Rank -> Tag -> Size a -> IO (a, Status)
recvImmut comm rank tag size = do
   recvToImmut size $ \arrayPtr numBytes ->
      alloca $ \statusPtr -> do
         checkError $ Internal.recv (castPtr arrayPtr) numBytes byte (fromRank rank) (fromTag tag) comm (castPtr statusPtr)
         peek statusPtr

bcastSend :: SendFrom a => Comm -> Rank -> a -> IO ()
bcastSend comm sendRank msg = do
   sendFrom msg $ \arrayPtr numBytes -> do
      checkError $ Internal.bcast (castPtr arrayPtr) numBytes byte (fromRank sendRank) comm

bcastRecvMut :: RecvToMut a => Comm -> Rank -> a -> IO ()
bcastRecvMut comm sendRank receptacle = do
   recvToMut receptacle $ \arrayPtr numBytes -> do
      checkError $ Internal.bcast (castPtr arrayPtr) numBytes byte (fromRank sendRank) comm

bcastRecvImmut :: RecvToImmut a => Comm -> Rank -> Size a -> IO a
bcastRecvImmut comm sendRank size = do
   fst <$> (recvToImmut size $ \arrayPtr numBytes ->
               checkError $ Internal.bcast (castPtr arrayPtr) numBytes byte (fromRank sendRank) comm)

isend, ibsend, issend :: SendFrom a => Comm -> Rank -> Tag -> a -> IO Request
isend  = isendWith Internal.isend
ibsend = isendWith Internal.ibsend
issend = isendWith Internal.issend

type ISendPrim = Ptr () -> CInt -> Datatype -> CInt -> CInt -> Comm -> Ptr (Request) -> IO CInt

isendWith :: SendFrom a => ISendPrim -> Comm -> Rank -> Tag -> a -> IO Request
isendWith send_function comm recvRank tag msg = do
   sendFrom msg $ \arrayPtr numBytes ->
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
{-
irecv :: (Storable e, Ix i) => Comm -> Rank -> Tag -> StorableArray i e -> IO Request
irecv comm sendRank tag array = do
   alloca $ \requestPtr ->
      recvInto array $ \arrayPtr numBytes -> do
         checkError $ Internal.irecv (castPtr arrayPtr) numBytes byte (fromRank sendRank) (fromTag tag) comm requestPtr
         peek requestPtr
-}

sendScatter :: (SendFrom a, RecvToMut b) => Comm -> Rank -> a -> b -> IO ()
sendScatter comm root msg receptacle = do
   recvToMut receptacle $ \recvPtr numBytes ->
     sendFrom msg $ \sendPtr _ ->
       checkError $ Internal.scatter (castPtr sendPtr) numBytes byte (castPtr recvPtr) numBytes byte (fromRank root) comm

recvScatterMut :: RecvToMut a => Comm -> Rank -> a -> IO ()
recvScatterMut comm root receptacle = do
   recvToMut receptacle $ \recvPtr numBytes ->
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
{-
sendScatterv :: (MessageArray a) => Comm -> Rank -> a ->
                 StorableArray Int Int -> StorableArray Int Int -> a -> IO ()
sendScatterv comm root sendArray counts displacements recvArray  = do
   -- myRank <- commRank comm
   -- XXX: assert myRank == sendRank ?
   recvInto recvArray $ \recvPtr numBytes ->
     sendFrom sendArray $ \sendPtr _ ->
       withStorableArray counts $ \countsPtr ->
         withStorableArray displacements $ \displPtr ->
           checkError $ Internal.scatterv (castPtr sendPtr) (castPtr countsPtr) (castPtr displPtr) byte
                        (castPtr recvPtr) numBytes byte (fromRank root) comm

recvScatterv :: (MessageArray a ) => Comm -> Rank -> a -> IO ()
recvScatterv comm root arr = do
   -- myRank <- commRank comm
   -- XXX: assert (myRank /= sendRank)
   recvInto arr $ \recvPtr numBytes ->
     checkError $ Internal.scatterv nullPtr nullPtr nullPtr byte (castPtr recvPtr) numBytes byte (fromRank root) comm
-}

{-
XXX we should check that the recvArray is large enough to store:

   segmentSize * commSize
-}
recvGatherMut :: (SendFrom a, RecvToMut b) => Comm -> Rank -> a -> b -> IO ()
recvGatherMut comm root segment receptacle = do
   -- myRank <- commRank comm
   -- XXX: assert myRank == root
   sendFrom segment $ \sendPtr segmentBytes ->
     recvToMut receptacle $ \recvPtr _ ->
       checkError $ Internal.gather (castPtr sendPtr) segmentBytes byte (castPtr recvPtr) segmentBytes byte
                    (fromRank root) comm

sendGather :: SendFrom a => Comm -> Rank -> a -> IO ()
sendGather comm root segment = do
   -- myRank <- commRank comm
   -- XXX: assert it is /= root
   sendFrom segment $ \sendPtr segmentBytes ->
     -- the recvPtr is ignored in this case, so we can make it NULL, likewise recvCount can be 0
     checkError $ Internal.gather (castPtr sendPtr) segmentBytes byte nullPtr 0 byte (fromRank root) comm

{-
recvGatherv :: (MessageArray a ) => Comm -> Rank -> a ->
                StorableArray Int Int -> StorableArray Int Int -> a -> IO ()
recvGatherv comm root segment counts displacements recvArray = do
   -- myRank <- commRank comm
   -- XXX: assert myRank == root
   sendFrom segment $ \sendPtr segmentBytes ->
     withStorableArray counts $ \countsPtr ->
        withStorableArray displacements $ \displPtr ->
          recvInto recvArray $ \recvPtr _ ->
            checkError $ Internal.gatherv (castPtr sendPtr) segmentBytes byte (castPtr recvPtr)
                         (castPtr countsPtr) (castPtr displPtr) byte (fromRank root) comm

sendGatherv :: (MessageArray a ) => Comm -> Rank -> a -> IO ()
sendGatherv comm root segment = do
   -- myRank <- commRank comm
   -- XXX: assert myRank == root
   sendFrom segment $ \sendPtr segmentBytes ->
     -- the recvPtr, counts and displacements are ignored in this case, so we can make it NULL
     checkError $ Internal.gatherv (castPtr sendPtr) segmentBytes byte nullPtr nullPtr nullPtr byte (fromRank root) comm
-}

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

recvWithMArrayAndSize :: forall i e r a b. (Storable e, Ix i, MArray a e IO) => a i e -> (Ptr b -> CInt -> IO r) -> IO r
recvWithMArrayAndSize array f = do
   bounds <- getBounds array
   let numElements = rangeSize bounds
   let elementSize = sizeOf (undefined :: e)
   let numBytes = cIntConv (numElements * elementSize)
   allocaArray numElements $ \ptr -> do
      result <- f (castPtr ptr) numBytes
      fillArrayFromPtr (range bounds) numElements ptr array
      return result

sendWithMArrayAndSize :: forall i e r a b. (Storable e, Ix i, MArray a e IO) => a i e -> (Ptr b -> CInt -> IO r) -> IO r
sendWithMArrayAndSize array f = do
   elements <- getElems array
   bounds <- getBounds array
   let numElements = rangeSize bounds
   let elementSize = sizeOf (undefined :: e)
   let numBytes = cIntConv (numElements * elementSize)
   withArray elements $ \ptr -> f (castPtr ptr) numBytes

-- XXX I wonder if this can be written without the intermediate list?
-- Maybe GHC can elimiate it. We should look at the generated compiled
-- code to see how well the loop is handled.
fillArrayFromPtr :: (MArray a e IO, Storable e, Ix i) => [i] -> Int -> Ptr e -> a i e -> IO ()
fillArrayFromPtr indices numElements startPtr array = do
   elems <- peekArray numElements startPtr
   mapM_ (\(index, element) -> writeArray array index element) (zip indices elems)

instance SendFrom Int where
   sendFrom = sendFromStorable

instance RecvToImmut Int where
   type Size Int = ()
   recvToImmut _ = recvToImmutStorable

{-
class SendFrom a where
   sendFrom :: a -> (Ptr b -> CInt -> IO c) -> IO c

class RecvToMut a where
   recvToMut :: a -> (Ptr b -> CInt -> IO c) -> IO c

class RecvToImmut a where
   type Size a :: *
   recvToImmut :: Size a -> (Ptr b -> CInt -> IO c) -> IO (a, c)
-}

sendFromStorable :: forall a b c. Storable a => a -> (Ptr b -> CInt -> IO c) -> IO c
sendFromStorable x f = do
   let numBytes = sizeOf (undefined :: a)
   alloca $ \ptr -> do
      poke ptr x
      f (castPtr ptr) (cIntConv numBytes)

recvToImmutStorable :: forall a b c . Storable a => (Ptr b -> CInt -> IO c) -> IO (a, c)
recvToImmutStorable f = do
   let numBytes = sizeOf (undefined :: a)
   alloca $ \ptr -> do
      result <- f (castPtr ptr) (cIntConv numBytes)
      val <- peek ptr
      return (val, result)

instance (Ix i, Storable e) => SendFrom (StorableArray i e) where
   sendFrom = withStorableArrayAndSize

instance (Ix i, Storable e) => RecvToMut (StorableArray i e) where
   recvToMut = withStorableArrayAndSize

withStorableArrayAndSize :: forall i e a b. (Storable e, Ix i) => StorableArray i e -> (Ptr b -> CInt -> IO a) -> IO a
withStorableArrayAndSize arr f = do
   rSize <- rangeSize <$> getBounds arr
   let numBytes = cIntConv (rSize * sizeOf (undefined :: e))
   withStorableArray arr $ \ptr -> f (castPtr ptr) numBytes

instance SendFrom ByteString where
   -- sendFrom :: ByteString -> (Ptr b -> CInt -> IO c) -> IO c
   sendFrom bs f =
      unsafeUseAsCString bs (\ptr -> f (castPtr ptr) $ cIntConv $ BS.length bs)

instance RecvToImmut ByteString where
   type Size ByteString = Int
   -- recvToImmut :: Int -> (Ptr b -> CInt -> IO c) -> IO (ByteString, c)
   recvToImmut count f = do
      allocaBytes count $ \ptr -> do
         result <- f ptr (cIntConv count)
         message <- packCStringLen (castPtr ptr, count)
         return (message, result)

instance (Ix i, Storable e) => SendFrom (IOArray i e) where
   sendFrom = sendWithMArrayAndSize

instance (Ix i, Storable e) => RecvToMut (IOArray i e) where
   recvToMut = recvWithMArrayAndSize

{-
XXX this does not work, because the compiler can't infer from (Storable e, Ix i)
    IOUArray i e, because only a select number of types can be elements of
    unboxed arrays. So sadly it appears that we have to enumerate them.

instance (Storable e, Ix i) => SendFrom IOUArray i e where
   sendFrom = sendWithMArrayAndSize

instance (Storable e, Ix i) => RecvToMut IOUArray i e where
   recvToMut = recvWithMArrayAndSize
-}

instance Ix i => SendFrom (IOUArray i Bool) where
   sendFrom = sendWithMArrayAndSize

instance Ix i => RecvToMut (IOUArray i Bool) where
   recvToMut = recvWithMArrayAndSize

instance Ix i => SendFrom (IOUArray i Char) where
   sendFrom = sendWithMArrayAndSize

instance Ix i => RecvToMut (IOUArray i Char) where
   recvToMut = recvWithMArrayAndSize

instance Ix i => SendFrom (IOUArray i Double) where
   sendFrom = sendWithMArrayAndSize

instance Ix i => RecvToMut (IOUArray i Double) where
   recvToMut = recvWithMArrayAndSize

instance Ix i => SendFrom (IOUArray i Float) where
   sendFrom = sendWithMArrayAndSize

instance Ix i => RecvToMut (IOUArray i Float) where
   recvToMut = recvWithMArrayAndSize

instance Ix i => SendFrom (IOUArray i Int) where
   sendFrom = sendWithMArrayAndSize

instance Ix i => RecvToMut (IOUArray i Int) where
   recvToMut = recvWithMArrayAndSize

instance Ix i => SendFrom (IOUArray i Int8) where
   sendFrom = sendWithMArrayAndSize

instance Ix i => RecvToMut (IOUArray i Int8) where
   recvToMut = recvWithMArrayAndSize

instance Ix i => SendFrom (IOUArray i Int16) where
   sendFrom = sendWithMArrayAndSize

instance Ix i => RecvToMut (IOUArray i Int16) where
   recvToMut = recvWithMArrayAndSize

instance Ix i => SendFrom (IOUArray i Int32) where
   sendFrom = sendWithMArrayAndSize

instance Ix i => RecvToMut (IOUArray i Int32) where
   recvToMut = recvWithMArrayAndSize

instance Ix i => SendFrom (IOUArray i Int64) where
   sendFrom = sendWithMArrayAndSize

instance Ix i => RecvToMut (IOUArray i Int64) where
   recvToMut = recvWithMArrayAndSize

instance Ix i => SendFrom (IOUArray i Word) where
   sendFrom = sendWithMArrayAndSize

instance Ix i => RecvToMut (IOUArray i Word) where
   recvToMut = recvWithMArrayAndSize

instance Ix i => SendFrom (IOUArray i Word8) where
   sendFrom = sendWithMArrayAndSize

instance Ix i => RecvToMut (IOUArray i Word8) where
   recvToMut = recvWithMArrayAndSize

instance Ix i => SendFrom (IOUArray i Word16) where
   sendFrom = sendWithMArrayAndSize

instance Ix i => RecvToMut (IOUArray i Word16) where
   recvToMut = recvWithMArrayAndSize

instance Ix i => SendFrom (IOUArray i Word32) where
   sendFrom = sendWithMArrayAndSize

instance Ix i => RecvToMut (IOUArray i Word32) where
   recvToMut = recvWithMArrayAndSize

instance Ix i => SendFrom (IOUArray i Word64) where
   sendFrom = sendWithMArrayAndSize

instance Ix i => RecvToMut (IOUArray i Word64) where
   recvToMut = recvWithMArrayAndSize

instance Ix i => SendFrom (IOUArray i (StablePtr a)) where
   sendFrom = sendWithMArrayAndSize

instance Ix i => RecvToMut (IOUArray i (StablePtr a)) where
   recvToMut = recvWithMArrayAndSize

instance Ix i => SendFrom (IOUArray i (Ptr a)) where
   sendFrom = sendWithMArrayAndSize

instance Ix i => RecvToMut (IOUArray i (Ptr a)) where
   recvToMut = recvWithMArrayAndSize

instance Ix i => SendFrom (IOUArray i (FunPtr a)) where
   sendFrom = sendWithMArrayAndSize

instance Ix i => RecvToMut (IOUArray i (FunPtr a)) where
   recvToMut = recvWithMArrayAndSize
