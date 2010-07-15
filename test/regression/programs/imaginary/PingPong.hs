module Main where

import Control.Monad (when)
import Bindings.MPI.Internal as MPI

main :: IO ()
main = do
   MPI.init
   (_, rank) <- MPI.commRank MPI.commWorld
   when (rank == 0) $ do
       MPI.send (0::Int) 1 MPI.int 1 42 MPI.commWorld
       ping
   when (rank == 1) pong
   MPI.finalize
   return ()

ping :: IO ()
ping = do
   (_, _, i) <- MPI.recv 1 MPI.int 1 42 MPI.commWorld 
   putStrLn $ "Ping " ++ show (i::Int)
   MPI.send (i+1) (1::Int) MPI.int 1 42 MPI.commWorld
   ping

pong :: IO ()
pong = do 
   (_, _, i) <- MPI.recv 1 MPI.int 0 42 MPI.commWorld 
   putStrLn $ "Pong " ++ show (i::Int)
   MPI.send (i+1) (1::Int) MPI.int 0 42 MPI.commWorld
   pong
