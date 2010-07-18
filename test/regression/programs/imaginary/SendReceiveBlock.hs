module Main where

import Prelude hiding (init)

import Control.Monad (when)
import Bindings.MPI 
import System.IO (BufferMode (NoBuffering), hSetBuffering, stdout)

data Rank = Sender | Receiver
   deriving (Enum, Eq)

data Tag = Tag
   deriving Enum

-- type Msg = (Bool, Int, String, [()])
type Msg = [Int] 

msg :: Msg 
-- msg = (True, 12, "fred", [(), (), ()])
msg = [1..5000000] 

main :: IO ()
main = do
   hSetBuffering stdout NoBuffering
   init
   rank <- commRank commWorld
   when (rank == Sender) $ do
      send msg Receiver Tag commWorld 
   when (rank == Receiver) $ do
      (_, result) <- recv Sender Tag commWorld
      busyWork 1000
      print (length (result :: Msg))
   finalize

busyWork :: Int -> IO ()
busyWork 0 = return ()
busyWork n = do
   print n
   busyWork (n-1)
