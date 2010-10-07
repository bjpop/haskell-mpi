-- Based on the example program from page 41/42 of 
-- Pacheco "Parallel programming with MPI"

module Main where

import Control.Parallel.MPI.Serializable
import Control.Parallel.MPI.Common

main :: IO ()
main = mpiWorld $ \_size rank ->
   if rank == 0
      then mapM_ putStrLn =<< (recvGather commWorld 0 $ msg rank)
      else sendGather commWorld 0 $ msg rank

msg :: Rank -> String
msg r = "Greetings from process " ++ show r ++ "!"
