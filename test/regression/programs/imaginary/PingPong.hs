module Main where

import Control.Monad (when)
import Control.Parallel.MPI

tag :: Tag
tag = toTag () 

data Actor = Pinger | Ponger
   deriving (Show, Enum, Eq)

pinger, ponger :: Rank
pinger = toRank Pinger
ponger = toRank Ponger

type Msg = Maybe Int

msg :: Msg 
msg = Just 0

main :: IO ()
main = mpi $ do
   size <- commSize commWorld
   when (size >= 2) $ do
      rank <- commRank commWorld
      when (rank == pinger) $ (send msg ponger tag commWorld >> act Pinger)
      when (rank == ponger) $ act Ponger

counterpart :: Actor -> Actor
counterpart Pinger = Ponger
counterpart Ponger = Pinger

act :: Actor -> IO ()
act actor = do
   let other = toRank $ counterpart actor
   (_status, msg) <- recv other tag commWorld 
   maybe 
      (return ())
      (\i -> if (i <= 1000) 
                then do putStrLn $ show actor ++ " " ++ show (i::Int)
                        send ((Just (i+1)) :: Msg) other tag commWorld
                        act actor 
                else send (Nothing :: Msg) other tag commWorld)
      msg
