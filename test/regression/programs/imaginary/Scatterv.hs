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
   myRank <- commRank commWorld

   let recvRange = (0, fromRank myRank)
   segment <- if myRank == root then do
     let bigRange@(low, hi) = (1, sum [1..numProcs])
     (msg :: StorMsg) <- newListArray bigRange [low..hi]
   
     let msgRange = (1, numProcs)
     (counts :: StorMsg) <- newListArray msgRange $ map (sizeOf (undefined::Int) *) [1..numProcs]
     (displs :: StorMsg) <- newListArray msgRange $ 0:(Prelude.init $ scanl1 (+) $ map (sizeOf (undefined::Int) *) [1..numProcs])
     
     sendScatterv msg counts displs recvRange root commWorld
     else recvScatterv recvRange root commWorld
   recvMsg <- getElems segment
   putStrLn $ "rank = " ++ show myRank ++ " segment = " ++ show recvMsg
