module Main where

import Control.Parallel.MPI.Serializable
import Control.Parallel.MPI.Common
import Data.Char (isDigit)
import Control.Applicative ((<$>))
import Control.Monad (when, forM)

main :: IO ()
main = mpiWorld $ \procs rank -> do
   n <- if (rank == zeroRank)
           then do
              input <- getNumber
              bcastSend commWorld zeroRank input
              return input
           else
              bcastRecv commWorld zeroRank
   let part = integrate (fromRank rank + 1) procs n (1 / fromIntegral n)
   send commWorld zeroRank unitTag part
   when (rank == zeroRank) $ do
      pi <- sum <$> gatherAll procs
      print pi

gatherAll :: Int -> IO [Double]
gatherAll procs =
   forM [0..procs-1] $ \rank ->
      fst <$> recv commWorld (toRank rank) unitTag

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

getNumber :: IO Int
getNumber = do
   line <- getLine
   if (all isDigit line)
      then return $ read line
      else return 0
