module Main where

import Prelude hiding (init)

import Control.Monad (when)
import Control.Parallel.MPI.Serializable
import Control.Parallel.MPI.Common

data Actor = Sender | Receiver
   deriving (Enum, Eq)

sender, receiver :: Rank
sender = toRank Sender
receiver = toRank Receiver

tag :: Tag
tag = toTag ()

type Msg = [Int]

msg :: Msg
msg = [1..5000000]

main :: IO ()
main = mpi $ do
   rank <- commRank commWorld
   when (rank == sender) $ do
      send msg receiver tag commWorld
   when (rank == receiver) $ do
      (_status, result) <- recv sender tag commWorld
      busyWork 1000
      print (length (result :: Msg))

busyWork :: Int -> IO ()
busyWork n = mapM_ print [1..n]
