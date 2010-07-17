-- Based on the example program from page 41/42 of 
-- Pacheco "Parallel programming with MPI"

module Main where

import Control.Monad (when, forM_)
import Bindings.MPI as MPI

data Tag = Tag deriving Enum

msg :: Int -> String 
msg r = "Greetings from process " ++ show r ++ "!"

comm :: Comm
comm = commWorld

main :: IO ()
main = do
   MPI.init
   rank <- commRank comm 
   size <- commSize comm 
   when (rank /= 0) $ 
      send (msg rank) 0 Tag comm 
   when (rank == 0) $ do
      forM_ [1..size-1] $ \sender -> do
         (_status, result) <- recv sender Tag comm 
         putStrLn result 
   finalize
