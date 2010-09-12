module Main where

import Control.Monad (when)
import Control.Parallel.MPI.StorableArray
import Control.Parallel.MPI.Common
import Data.Array.Storable (StorableArray, newListArray, getElems)

data Actor = Sender | Receiver
   deriving (Enum, Eq)

sender, receiver :: Rank
sender = toRank Sender
receiver = toRank Receiver

tag :: Tag
tag = toTag ()

type Msg = StorableArray Int Int

range :: (Int, Int)
range@(low, hi) = (1,50000)

msg :: IO Msg
msg = newListArray range [low..hi]

main :: IO ()
main = mpi $ do
   rank <- commRank commWorld
   when (rank == sender) $ do
      array <- msg
      isend array receiver tag commWorld
      return ()
   when (rank == receiver) $ do
      (array, request) <- irecv range sender tag commWorld
      busyWork request 0
      msg <- getElems (array :: Msg)
      print $ length msg

busyWork :: Request -> Int -> IO ()
busyWork request i = do
    maybeStatus <- test request
    case maybeStatus of
       Nothing -> do
          print i
          busyWork request (i+1)
       Just _ -> return ()
