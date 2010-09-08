{-# LANGUAGE FlexibleContexts, ScopedTypeVariables #-}

module Control.Parallel.MPI.StorableArray
   ( send
   , recv
   , bcast
   , iSend
   , iRecv
   ) where

import C2HS
import Data.Array.Storable
import qualified Control.Parallel.MPI.Internal as Internal
import Control.Parallel.MPI.Datatype as Datatype
import Control.Parallel.MPI.Comm as Comm
import Control.Parallel.MPI.Status as Status
import Control.Parallel.MPI.Utils (checkError)
import Control.Parallel.MPI.Tag as Tag
import Control.Parallel.MPI.Rank as Rank
import Control.Parallel.MPI.Request as Request
import Control.Parallel.MPI.Common (commRank)

send :: forall e i. (Storable e, Ix i) => StorableArray i e -> Rank -> Tag -> Comm -> IO ()
send array rank tag comm = do
   bounds <- getBounds array
   let arraySize = rangeSize bounds
       cRank = fromRank rank
       cTag  = fromTag tag
       elementSize = sizeOf (undefined :: e)
       numBytes = cIntConv (arraySize * elementSize)
   withStorableArray array $ \arrayPtr -> do
      checkError $ Internal.send (castPtr arrayPtr) numBytes byte cRank cTag comm

recv :: forall e . Storable e => Int -> Rank -> Tag -> Comm -> IO (Status, StorableArray Int e)
recv numElements rank tag comm = do
   let cRank = fromRank rank
       cTag = fromTag tag
       elementSize = sizeOf (undefined :: e)
       cBytes = cIntConv (numElements * elementSize)
   foreignPtr <- mallocForeignPtrArray numElements
   withForeignPtr foreignPtr $ \arrayPtr ->
      alloca $ \statusPtr -> do
         checkError $ Internal.recv (castPtr arrayPtr) cBytes byte cRank cTag comm (castPtr statusPtr)
         recvStatus <- peek statusPtr
         storableArray <- unsafeForeignPtrToStorableArray foreignPtr (0, numElements-1)
         return (recvStatus, storableArray)

bcast :: forall e . Storable e => StorableArray Int e -> Int -> Rank -> Comm -> IO (StorableArray Int e)
bcast array numElements sendRank comm = do
   myRank <- commRank comm
   let cRank = fromRank sendRank
       elementSize = sizeOf (undefined :: e)
       cBytes = cIntConv (numElements * elementSize)
   if myRank == sendRank
      then withStorableArray array $ \arrayPtr -> do
              checkError $ Internal.bcast (castPtr arrayPtr) cBytes byte cRank comm
              return array
      else do
         foreignPtr <- mallocForeignPtrArray numElements
         withForeignPtr foreignPtr $ \arrayPtr -> do
            checkError $ Internal.bcast (castPtr arrayPtr) cBytes byte cRank comm
            unsafeForeignPtrToStorableArray foreignPtr (0, numElements-1)

iSend :: forall e . Storable e => StorableArray Int e -> Int -> Rank -> Tag -> Comm -> IO Request
iSend array numElements recvRank tag comm = do
   let cRank = fromRank recvRank
       cTag  = fromTag tag
       elementSize = sizeOf (undefined :: e)
       cBytes = cIntConv (numElements * elementSize)
   alloca $ \requestPtr ->
      withStorableArray array $ \arrayPtr -> do
         checkError $ Internal.iSend (castPtr arrayPtr) cBytes byte cRank cTag comm requestPtr
         peek requestPtr

iRecv :: forall e . Storable e => Int -> Rank -> Tag -> Comm -> IO (StorableArray Int e, Request)
iRecv numElements sendRank tag comm = do
   let cRank = fromRank sendRank
       cTag  = fromTag tag
       elementSize = sizeOf (undefined :: e)
       cBytes = cIntConv (numElements * elementSize)
   alloca $ \requestPtr -> do
      foreignPtr <- mallocForeignPtrArray numElements
      withForeignPtr foreignPtr $ \arrayPtr -> do
         checkError $ Internal.iRecv (castPtr arrayPtr) cBytes byte cRank cTag comm requestPtr
         array <- unsafeForeignPtrToStorableArray foreignPtr (0, numElements-1)
         request <- peek requestPtr
         return (array, request)
