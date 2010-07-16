module Main where

import Control.Monad (when)
import Bindings.MPI.Internal as MPI
import Bindings.MPI as SMPI
import Bindings.MPI.Comm as MPI

main :: IO ()
main = do
   MPI.init
   (_, rank) <- MPI.commRank MPI.commWorld
   when (rank == 0) $ do
      SMPI.send (True,42::Int,"fred",[(), (), ()]) 1 42 MPI.commWorld 
      return ()
   when (rank == 1) $ do
      (_, status, result) <- SMPI.recv 0 42 MPI.commWorld
      print (result :: (Bool, Int, String, [()]))
      print status
   MPI.finalize
   return ()
