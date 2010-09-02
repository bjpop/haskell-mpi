-- Based on the example program from page 41/42 of 
-- Pacheco "Parallel programming with MPI"

module Main where

import Control.Monad (when, forM)
import Control.Parallel.MPI.Serializable

msg :: Rank -> String 
msg r = "Greetings from process " ++ show r ++ "!"

comm :: Comm
comm = commWorld

root :: Rank
root = toRank 0

tag :: Tag
tag = toTag ()

main :: IO ()
main = mpi $ do
   rank <- commRank comm 
   size <- commSize comm 
   if (rank /= root) 
      then send (msg rank) root tag comm 
      else do 
         futures <- forM [1..size-1] $ \sender -> 
            recvFuture (toRank sender) tag comm 
         mapM_ printFuture futures 
    where
    printFuture :: Future String -> IO ()
    printFuture future = putStrLn =<< waitFuture future
            
