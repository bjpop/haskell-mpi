module Main where

import Prelude hiding (init)
import Control.Monad (when)
import Bindings.MPI

tag :: Int
tag = 42

data Rank = Pinger | Ponger
   deriving (Enum, Eq)

main :: IO ()
main = do
   init
   (_, rank) <- commRank commWorld
   send (0::Integer) Ponger tag commWorld
   when (toEnum rank == Pinger) ping
   when (toEnum rank == Ponger) pong
   finalize
   return ()

ping :: IO ()
ping = do
   (_, _, i) <- recv Ponger tag commWorld 
   putStrLn $ "Ping " ++ show (i::Integer)
   send (i+1) Ponger tag commWorld
   ping

pong :: IO ()
pong = do 
   (_, _, i) <- recv Pinger tag commWorld 
   putStrLn $ "Pong " ++ show (i::Integer)
   send (i+1) Pinger tag commWorld
   pong
