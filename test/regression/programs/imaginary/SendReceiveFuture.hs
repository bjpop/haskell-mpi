module Main where

import Control.Monad (when)
import Control.Parallel.MPI.Serializable

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
      future <- recvFuture sender tag commWorld
      busyWork 1000
      result <- waitFuture future 
      print (length (result :: Msg))

busyWork :: Int -> IO ()
busyWork 0 = return ()
busyWork n = do
   print n
   busyWork (n-1)
