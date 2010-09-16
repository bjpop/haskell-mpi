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
   let segRange@(low,hi) = (1, segmentSize)
   (msg :: StorMsg) <- newListArray segRange [low..hi]
   let bigRange = (1, segmentSize * numProcs)
   result <- gather msg bigRange root commWorld
   myRank <- commRank commWorld
   recvMsg <- getElems result
   putStrLn $ "rank = " ++ show myRank ++ " result = " ++ show recvMsg
