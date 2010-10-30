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
import Text.Printf

main :: IO ()
main = mpiWorld $ \size rank -> do
   let root = 0
   n <- if rank == root
           then do
              input <- getNumber
              bcastSend commWorld root input
              return input
           else
              bcastRecv commWorld root
   let part = integrate (fromRank rank + 1) size n (1 / fromIntegral n)
   if rank == root
      then do
         parts <- gatherRecv commWorld root part
         printf "%1.8f\n" $ sum parts
      else
         gatherSend commWorld root part

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
