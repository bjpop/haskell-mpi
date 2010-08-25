{-# LANGUAGE FlexibleContexts, ScopedTypeVariables #-}

module Control.Parallel.MPI.StorableArray
   ( send
   , recv
--   , iSend
--   , bcast
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

send :: Ix i => StorableArray i e -> Rank -> Tag -> Comm -> IO ()
send array rank tag comm = do
   bounds <- getBounds array
   let arraySize = rangeSize bounds
       cRank = fromRank rank
       cTag  = fromTag tag
       elementSize = sizeOf (undefined :: element)
       numBytes = cIntConv (arraySize * elementSize)
   allocaArray arraySize $ \arrayPtr -> do
      elems <- getElems array
      pokeArray arrayPtr elems
      checkError $ Internal.send (castPtr arrayPtr) numBytes byte cRank cTag comm

recv :: (Storable element, MArray array element IO) => Int -> Rank -> Tag -> Comm -> IO (Status, array Int element)
recv numElements rank tag comm = do
--   probeStatus <- probe rank tag comm
--   let byteCount = status_count probeStatus 
   let elementSize = sizeOf (undefined :: element)
       -- numElements = byteCount `div` elementSize
       byteCount = numElements * elementSize
       cSource = fromRank rank 
       cTag    = fromTag tag
       cCount  = cIntConv byteCount 
   allocaBytes byteCount 
      (\bufferPtr -> 
          alloca $ \statusPtr -> do
             checkError $ Internal.recv (castPtr bufferPtr) cCount byte cSource cTag comm $ castPtr statusPtr
             recvStatus <- peek statusPtr
             messageList <- peekArray numElements bufferPtr
             messageArray <- newListArray (0, numElements - 1) messageList
             return (recvStatus, messageArray))
