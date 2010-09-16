{-# LANGUAGE ScopedTypeVariables #-}
module StorableArrayTests (storableArrayTests) where

import TestHelpers
import Control.Parallel.MPI.StorableArray
import Data.Array.Storable (StorableArray, newListArray, getElems, getBounds)

import Foreign.Storable


storableArrayTests :: Rank -> [(String,TestRunnerTest)]
storableArrayTests rank =
  [ mpiTestCase rank "send+recv array" syncSendRecvTest
  , mpiTestCase rank "Broadcast array" broadcastTest
  , mpiTestCase rank "Scatter array"   scatterTest
  , mpiTestCase rank "Scatterv array"  scattervTest
  , mpiTestCase rank "Gather array"    gatherTest    
  ]
syncSendRecvTest, broadcastTest, scatterTest, scattervTest, gatherTest :: Rank -> IO ()

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


scatterTest _ = do
  numProcs <- commSize commWorld
  let bigRange@(low, hi) = (1, segmentSize * numProcs)
  (msg :: ArrMsg) <- newListArray bigRange [low..hi]
  let segRange = (1, segmentSize)
  segment <- scatter msg segRange zeroRank commWorld
  myRank <- commRank commWorld
  recvMsg <- getElems segment
  recvMsg == take 10 [(fromRank myRank)*10+1..] @? "Rank " ++ show myRank ++ " got segment " ++ show recvMsg
  where
    segmentSize = 10

-- scatter list [1..] in a way such that:
-- rank 0 will receive [1]
-- rank 1 will receive [2,3]
-- rank 2 will receive [3,4,5]
-- rank 3 will receive [6,7,8,9]
-- etc
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
  
gatherTest _ = do
  numProcs <- commSize commWorld
  let segRange@(low,hi) = (1, segmentSize)
  (msg :: ArrMsg) <- newListArray segRange [low..hi]
  let bigRange = (1, segmentSize * numProcs)
  result <- gather msg bigRange zeroRank commWorld
  myRank <- commRank commWorld
  recvMsg <- getElems result
  ( recvMsg == if myRank == zeroRank 
               then concat $ replicate numProcs [1..segmentSize]
               else [1..segmentSize] ) @? "Rank " ++ show myRank ++ " got " ++ show recvMsg
  where segmentSize = 10
   
