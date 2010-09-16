{-# LANGUAGE ScopedTypeVariables #-}
module Main where

import Control.Monad (when)
import Control.Parallel.MPI.StorableArray as Stor
import Control.Parallel.MPI.Common
import Data.Array.Storable (StorableArray, newListArray, getElems)
import Foreign.Storable

type StorMsg = StorableArray Int Int

root :: Rank
root = toRank 0

main :: IO ()
main = mpi $ do
   numProcs <- commSize commWorld
   let bigRange@(low, hi) = (1, sum [1..numProcs])
   (msg :: StorMsg) <- newListArray bigRange [low..hi]
   
   myRank <- commRank commWorld
   let msgRange = (1, numProcs)
   (counts :: StorMsg) <- newListArray msgRange $ map (sizeOf (undefined::Int) *) [1..numProcs]
   (displs :: StorMsg) <- newListArray msgRange $ 0:(Prelude.init $ scanl1 (+) $ map (sizeOf (undefined::Int) *) [1..numProcs])
   
   let recvRange = (0, fromRank myRank)
   segment <- scatterv msg counts displs recvRange root commWorld
   recvMsg <- getElems segment
   putStrLn $ "rank = " ++ show myRank ++ " segment = " ++ show recvMsg
