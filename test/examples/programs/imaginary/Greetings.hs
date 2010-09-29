-- Based on the example program from page 41/42 of 
-- Pacheco "Parallel programming with MPI"

module Main where

import Control.Monad
import Control.Applicative
import Control.Parallel.MPI.Serializable
import Control.Parallel.MPI.Common

main :: IO ()
main = mpiWorld $ \size rank ->
   if rank /= zeroRank
      then send commWorld zeroRank unitTag $ msg rank
      else forM_ [1..size-1] $ \sender ->
              putStrLn =<< fst <$> recv commWorld (toRank sender) unitTag

msg :: Rank -> String
msg r = "Greetings from process " ++ show r ++ "!"
