-- Test sending and receiving a nested data structure 
-- of some arbitrary size and shape.

module Main where

import Control.Monad (when)
import Bindings.MPI 

data Actor = Sender | Receiver
   deriving (Enum, Eq)

data Tag = Tag
   deriving Enum

type Msg = (Bool, Int, String, [()])

msg :: Msg 
msg = (True, 12, "fred", [(), (), ()])

sender, receiver :: Rank
sender = toRank Sender
receiver = toRank Receiver

main :: IO ()
main = mpi $ do 
   size <- commSize commWorld
   when (size >= 2) $ do
      rank <- commRank commWorld
      when (rank == sender) $ do
         send msg receiver Tag commWorld 
      when (rank == receiver) $ do
         (_status, result) <- recv sender Tag commWorld
         print (result :: Msg)
