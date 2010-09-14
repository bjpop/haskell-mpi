{-# LANGUAGE ScopedTypeVariables #-}
module Main where

import Control.Monad (when)
import Control.Parallel.MPI.StorableArray as Stor
import Control.Parallel.MPI.Common
import Data.Array.Storable (StorableArray, newListArray, getElems)

type StorMsg = StorableArray Int Int

segmentSize :: Int
segmentSize = 10

root :: Rank
root = toRank 0

main :: IO ()
main = mpi $ do
   numProcs <- commSize commWorld
   let bigRange@(low, hi) = (1, segmentSize * numProcs)
   (msg :: StorMsg) <- newListArray bigRange [low..hi]
   let segRange = (1, segmentSize)
   segment <- scatter msg segRange root commWorld
   myRank <- commRank commWorld
   recvMsg <- getElems segment
   putStrLn $ "rank = " ++ show myRank ++ " segment = " ++ show recvMsg
