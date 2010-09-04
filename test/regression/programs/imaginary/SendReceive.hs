-- Test sending and receiving a nested data structure 
-- of some arbitrary size and shape.

module Main where

import Control.Monad (when)
import Control.Parallel.MPI.Serializable
import Control.Parallel.MPI.Common
import Control.Concurrent

data Actor = Sender | Receiver
   deriving (Enum, Eq)

tag :: Tag
tag = toTag ()

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
         send msg receiver tag commWorld
      when (rank == receiver) $ do
         (_status, result) <- recv sender tag commWorld
         threadDelay (10*10^6)
         print (result :: Msg)
