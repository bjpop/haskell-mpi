module Main where

import Prelude hiding (init)

import Control.Monad (when)
import Bindings.MPI 

data Actor = Sender | Receiver
   deriving (Enum, Eq)

sender, receiver :: Rank
sender = toRank Sender
receiver = toRank Receiver

data Tag = Tag
   deriving Enum

type Msg = [Int] 

msg :: Msg 
msg = [1..5000000] 

main :: IO ()
main = mpi $ do 
   rank <- commRank commWorld
   when (rank == sender) $ do
      send msg receiver Tag commWorld 
   when (rank == receiver) $ do
      (_status, result) <- recv sender Tag commWorld
      busyWork 1000
      print (length (result :: Msg))

busyWork :: Int -> IO ()
busyWork 0 = return ()
busyWork n = do
   print n
   busyWork (n-1)
