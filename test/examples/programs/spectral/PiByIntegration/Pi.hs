module Main where

import Control.Parallel.MPI.Serializable
import Control.Parallel.MPI.Common
import Data.Char (isDigit)
import System.IO (hFlush, stdout)
import Control.Applicative ((<$>))
import Control.Monad (when, forM)

main :: IO ()
main = mpiWorld $ \procs rank -> do
   n <- if (rank == zeroRank)
           then do
              input <- getNumber "Enter number of intervals: "
              bcast input zeroRank commWorld
           else
              bcast undefined zeroRank commWorld
   let part = integrate (fromRank rank + 1) procs n (1 / fromIntegral n)
   send part zeroRank unitTag commWorld
   when (rank == zeroRank) $ do
      pi <- sum <$> gatherAll procs
      print pi

gatherAll :: Int -> IO [Double]
gatherAll procs =
   forM [0..procs-1] $ \rank ->
      snd <$> recv (toRank rank) unitTag commWorld

integrate :: Int -> Int -> Int -> Double -> Double
integrate rank procs n h =
   -- XXX superfluous type annotation needed to work around
   -- suspected GHC bug, see ticket #4321
   -- http://hackage.haskell.org/trac/ghc/ticket/4321
   -- (nothng to do with MPI)
   h * (sum (map area steps) :: Double)
   where
   steps = [rank, rank + procs .. n]
   area :: Int -> Double
   area i
      = 4 / (1 + x * x)
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
