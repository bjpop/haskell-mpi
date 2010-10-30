-- Based on the example program from page 41/42 of 
-- Pacheco "Parallel programming with MPI"

module Main where

import Control.Parallel.MPI.Serializable
import Control.Parallel.MPI.Common

main :: IO ()
main = mpiWorld $ \_size rank -> do
   let root = 0
   if rank == root
      then mapM_ putStrLn =<< (gatherRecv commWorld root $ msg rank)
      else gatherSend commWorld root $ msg rank

msg :: Rank -> String
msg r = "Greetings from process " ++ show r ++ "!"
