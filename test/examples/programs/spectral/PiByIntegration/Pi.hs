module Main where

import Control.Parallel.MPI.Serializable
import Control.Parallel.MPI.Common
import Data.Char (isDigit)
import System.IO (hFlush, stdout)
import Data.Array.Storable
import Control.Applicative ((<$>))
import Control.Monad (when, forM)

root :: Rank
root = toRank 0

tag :: Tag
tag = toTag ()

main :: IO ()
main = mpi $ do
   rank <- commRank commWorld
   numProcs <- commSize commWorld
   n <- if (rank == root)
           then do
              input <- getNumber "Enter number of intervals: "
              bcast input root commWorld
           else
              bcast undefined root commWorld
   let part = integrate (fromRank rank + 1) numProcs n (1 / fromIntegral n)
   send part root tag commWorld
   when (rank == root) $ do
      pi <- sum <$> gatherAll numProcs
      print pi

gatherAll :: Int -> IO [Double]
gatherAll numProcs =
   forM [0..numProcs-1] $ \rank ->
      snd <$> recv (toRank rank) tag commWorld

integrate :: Int -> Int -> Int -> Double -> Double
integrate rank numProcs n h =
   -- XXX superfluous type annotation needed to work around
   -- suspected GHC bug, see ticket #4321
   -- http://hackage.haskell.org/trac/ghc/ticket/4321
   -- (nothng to do with MPI)
   h * (sum (map rectangle steps) :: Double)
   where
   steps = [rank, rank + numProcs .. n]
   rectangle :: Int -> Double
   rectangle i
      = 4 / (1 + x*x)
      where
      x = h * (fromIntegral i - 0.5)

getNumber :: String -> IO Int
getNumber prompt = do
   putStr prompt
   hFlush stdout
   line <- getLine
   if (all isDigit line)
      then return $ read line
      else return 0
