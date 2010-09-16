{-# LANGUAGE ScopedTypeVariables #-}
module StorableArrayTests (storableArrayTests) where

import TestHelpers
import Control.Parallel.MPI.StorableArray
import Data.Array.Storable (StorableArray, newListArray, getElems, getBounds)

import Foreign.Storable


storableArrayTests :: Rank -> [(String,TestRunnerTest)]
storableArrayTests rank =
  [ mpiTestCase rank "Sending (sync)/receiving (sync) simple array" syncSendRecvTest
  , mpiTestCase rank "Broadcast array" broadcastTest
  , mpiTestCase rank "Scatterv array" scattervTest
  ]
syncSendRecvTest, broadcastTest, scattervTest :: Rank -> IO ()

-- StorableArray tests
type ArrMsg = StorableArray Int Int

low,hi :: Int
range :: (Int, Int)
range@(low,hi) = (1,10)

arrMsg :: IO ArrMsg
arrMsg = newListArray range [low..hi]

syncSendRecvTest rank
  | rank == sender   = do msg <- arrMsg
                          send msg receiver tag2 commWorld
  | rank == receiver = do (status, newMsg) <- recv range sender tag2 commWorld
                          checkStatus status sender tag2
                          elems <- getElems newMsg
                          elems == [low..hi::Int] @? "Got wrong array: " ++ show elems
  | otherwise        = return ()

broadcastTest _ = do
  msg <- arrMsg
  bs <- getBounds msg
  newMsg <- bcast (msg :: ArrMsg) bs sender commWorld
  elems <- getElems msg
  newElems <- getElems newMsg
  elems == newElems @? "StorableArray bcast yielded garbled result: " ++ show newElems

scattervTest _ = do
  numProcs <- commSize commWorld
  let bigRange@(low, hi) = (1, sum [1..numProcs])
  (msg :: ArrMsg) <- newListArray bigRange [low..hi]
   
  myRank <- commRank commWorld
  let myRankNo = fromRank myRank
      msgRange = (1, numProcs)
      counts = [1..numProcs]
      displs = (0:(Prelude.init $ scanl1 (+) $ [1..numProcs]))
  (packCounts :: ArrMsg) <- newListArray msgRange $ map (sizeOf (undefined::Int) *) counts
  (packDispls :: ArrMsg) <- newListArray msgRange $ map (sizeOf (undefined::Int) *) displs
  
  let recvRange = (0, myRankNo)
  segment <- scatterv msg packCounts packDispls recvRange zeroRank commWorld
  recvMsg <- getElems segment
  
  let myCount = counts!!myRankNo
      myDispl = displs!!myRankNo
      expected = take myCount $ drop myDispl [low..hi]
  recvMsg == expected @? "rank = " ++ show myRank ++ " got segment = " ++ show recvMsg ++ " instead of " ++ show expected