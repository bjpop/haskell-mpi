module Main where

import Control.Monad (when)
import Control.Parallel.MPI.Serializable as Ser
import Control.Parallel.MPI.StorableArray as Stor
import Control.Parallel.MPI.Common
import Data.Array.Storable (StorableArray, newListArray, getElems)

data Actor = Sender | Receiver
   deriving (Enum, Eq)

tag :: Tag
tag = toTag ()

type SerMsg = (Bool, Int, String, [()])

serMsg :: SerMsg
serMsg = (True, 12, "fred", [(), (), ()])

type StorMsg = StorableArray Int Int

bounds :: (Int, Int)
bounds@(low,hi) = (1,50)

storMsg :: IO StorMsg
storMsg = newListArray bounds [low..hi]

sender, receiver :: Rank
sender = toRank Sender
receiver = toRank Receiver

main :: IO ()
main = mpi $ do
   size <- commSize commWorld
   when (size >= 2) $ do
      rank <- commRank commWorld
      when (rank == sender) $ do
         Ser.send serMsg receiver tag commWorld
         array <- storMsg
         Stor.send array receiver tag commWorld
      when (rank == receiver) $ do
         (_status, result) <- Ser.recv sender tag commWorld
         print (result :: SerMsg)
         (_status, result) <- Stor.recv bounds sender tag commWorld
         array <- getElems result
         print (array :: [Int])
