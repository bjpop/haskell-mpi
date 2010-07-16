module Main where

import Prelude hiding (init)
import Control.Monad (when)
import Bindings.MPI

tag :: Int
tag = 42

data Rank = Pinger | Ponger
   deriving (Enum, Eq)

msg :: Integer
msg = 0

main :: IO ()
main = do
   init
   (_, rank) <- commRank commWorld
   send msg Ponger tag commWorld
   when (rank == Pinger) ping
   when (rank == Ponger) pong
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
