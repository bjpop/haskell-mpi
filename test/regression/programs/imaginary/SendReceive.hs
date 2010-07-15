module Main where

import Control.Monad (when)
import Bindings.MPI.Internal as MPI

main :: IO ()
main = do
   MPI.init
   (_, rank) <- MPI.commRank MPI.commWorld
   when (rank == 0) $ do
      _ <- MPI.send (12 :: Int) 1 MPI.int 1 42 MPI.commWorld 
      return ()
   when (rank == 1) $ do
      (_, status, result) <- MPI.recv 1 MPI.int 0 42 MPI.commWorld 
      print (result :: Int)
      print status
   _ <- MPI.finalize
   return ()
