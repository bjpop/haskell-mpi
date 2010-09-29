{-
   This program calculates Pi by integrating
   f(x) = 4 / (1 + x^2)
   in the range 0 <= x <= 1.

   It is not a particularly clever or efficient way
   to caculuate Pi. Rather it is intended to demonstrate
   a simple use of MPI.
-}

module Main where

import Control.Parallel.MPI.Serializable
import Control.Parallel.MPI.Common
import Data.Char (isDigit)
import Control.Applicative
import Control.Monad

main :: IO ()
main = mpiWorld $ \size rank -> do
   n <- if rank == zeroRank
           then do
              input <- getNumber
              bcast commWorld zeroRank input
           else
              bcast commWorld zeroRank undefined
   let part = integrate (fromRank rank + 1) size n (1 / fromIntegral n)
   send commWorld zeroRank unitTag part
   when (rank == zeroRank) $
      print =<< sum <$> gatherAll size

-- XXX This should be replaced by a true MPI gather
gatherAll :: Int -> IO [Double]
gatherAll size =
   forM [0..size-1] $ \rank ->
      fst <$> recv commWorld (toRank rank) unitTag

integrate :: Int -> Int -> Int -> Double -> Double
integrate rank size n h =
   -- XXX superfluous type annotation needed to work around
   -- confirmed GHC bug, see ticket #4321
   -- http://hackage.haskell.org/trac/ghc/ticket/4321
   -- (nothng to do with MPI)
   h * (sum (map area steps) :: Double)
   where
   steps = [rank, rank + size .. n]
   area :: Int -> Double
   area i
      = 4 / (1 + x * x)
      where
      x = h * (fromIntegral i - 0.5)

getNumber :: IO Int
getNumber = do
   line <- getLine
   if all isDigit line
      then return $ read line
      else return 0
