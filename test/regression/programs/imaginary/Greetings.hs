-- Based on the example program from page 41/42 of 
-- Pacheco "Parallel programming with MPI"

module Main where

import Control.Monad (when, forM_)
import Bindings.MPI as MPI

data Tag = Tag deriving Enum

msg :: Rank -> String 
msg r = "Greetings from process " ++ show r ++ "!"

comm :: Comm
comm = commWorld

root :: Rank
root = toRank 0

main :: IO ()
main = mpi $ do
   rank <- commRank comm 
   size <- commSize comm 
   if (rank /= root) 
      then send (msg rank) root Tag comm 
      else do forM_ [1..size-1] $ \sender -> do
              (_status, result) <- recv (toRank sender) Tag comm 
              putStrLn result 
