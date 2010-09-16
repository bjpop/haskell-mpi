module StorableArrayTests (storableArrayTests) where

import TestHelpers
import Control.Parallel.MPI.StorableArray
import Data.Array.Storable (StorableArray, newListArray, getElems, getBounds)


storableArrayTests :: Rank -> [(String,TestRunnerTest)]
storableArrayTests rank =
  [ mpiTestCase rank "Sending (sync)/receiving (sync) simple array" arraySyncRecv
  , mpiTestCase rank "Broadcast array" arrayBroadcast
  ]
arraySyncRecv, arrayBroadcast :: Rank -> IO ()

-- StorableArray tests
type ArrMsg = StorableArray Int Int

low,hi :: Int
range :: (Int, Int)
range@(low,hi) = (1,10)

arrMsg :: IO ArrMsg
arrMsg = newListArray range [low..hi]

arraySyncRecv rank
  | rank == sender   = do msg <- arrMsg
                          send msg receiver tag2 commWorld
  | rank == receiver = do (status, newMsg) <- recv range sender tag2 commWorld
                          checkStatus status sender tag2
                          elems <- getElems newMsg
                          elems == [low..hi::Int] @? "Got wrong array: " ++ show elems
  | otherwise        = return ()

arrayBroadcast _ = do
  msg <- arrMsg
  bs <- getBounds msg
  newMsg <- bcast (msg :: ArrMsg) bs sender commWorld
  elems <- getElems msg
  newElems <- getElems newMsg
  elems == newElems @? "StorableArray bcast yielded garbled result: " ++ show newElems

-- End of StorableArray tests
