module Main where

import Prelude hiding (init)

import Control.Monad (when)
import Bindings.MPI 

data Rank = Sender | Receiver
   deriving (Enum, Eq)

data Tag = Tag
   deriving Enum

type Msg = (Bool, Int, String, [()])

msg :: Msg 
msg = (True, 12, "fred", [(), (), ()])

main :: IO ()
main = do
   init
   (_, rank) <- commRank commWorld
   when (rank == Sender) $ do
      send msg Receiver Tag commWorld 
      return ()
   when (rank == Receiver) $ do
      (_, _status, result) <- recv Sender Tag commWorld
      print (result :: Msg)
   finalize
   return ()
