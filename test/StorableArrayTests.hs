{-# LANGUAGE ScopedTypeVariables #-}
module StorableArrayTests (storableArrayTests) where

import TestHelpers
import Control.Parallel.MPI.StorableArray
import Data.Array.Storable (StorableArray, newListArray, getElems, getBounds)

import Foreign.Storable
import Control.Concurrent (threadDelay)
import Control.Monad (when)

storableArrayTests :: Rank -> [(String,TestRunnerTest)]
storableArrayTests rank =
  [ mpiTestCase rank "send+recv array"  $ syncSendRecvTest send
  , mpiTestCase rank "ssend+recv array" $ syncSendRecvTest ssend
  , mpiTestCase rank "rsend+recv array" $ rsendRecvTest
  , mpiTestCase rank "isend+irecv array"  $ asyncSendRecvTest isend
  , mpiTestCase rank "issend+irecv array" $ asyncSendRecvTest issend
  , mpiTestCase rank "broadcast array" broadcastTest
  , mpiTestCase rank "scatter array"   scatterTest
  , mpiTestCase rank "scatterv array"  scattervTest
  , mpiTestCase rank "gather array"    gatherTest
  , mpiTestCase rank "gatherv array"   gathervTest
  ]
syncSendRecvTest  :: (StorableArray Int Int -> Rank -> Tag -> Comm -> IO ()) -> Rank -> IO ()
asyncSendRecvTest :: (StorableArray Int Int -> Rank -> Tag -> Comm -> IO Request) -> Rank -> IO ()
rsendRecvTest, broadcastTest, scatterTest, scattervTest, gatherTest, gathervTest :: Rank -> IO ()

-- StorableArray tests
type ArrMsg = StorableArray Int Int

low,hi :: Int
range :: (Int, Int)
range@(low,hi) = (1,10)

arrMsg :: IO ArrMsg
arrMsg = newListArray range [low..hi]

syncSendRecvTest sendf rank
  | rank == sender   = do msg <- arrMsg
                          sendf msg receiver tag2 commWorld
  | rank == receiver = do (status, newMsg) <- recv range sender tag2 commWorld
                          checkStatus status sender tag2
                          elems <- getElems newMsg
                          elems == [low..hi::Int] @? "Got wrong array: " ++ show elems
  | otherwise        = return ()

rsendRecvTest rank = do
  when (rank == receiver) $ do (status, newMsg) <- recv range sender tag2 commWorld
                               checkStatus status sender tag2
                               elems <- getElems newMsg
                               elems == [low..hi::Int] @? "Got wrong array: " ++ show elems
  when (rank == sender)   $ do msg <- arrMsg
                               threadDelay (2* 10^(6 :: Integer))
                               rsend msg receiver tag2 commWorld
  return ()

asyncSendRecvTest isendf rank
  | rank == sender   = do msg <- arrMsg
                          req <- isendf msg receiver tag3 commWorld
                          stat <- wait req
                          checkStatus stat sender tag3
  | rank == receiver = do (newMsg, req) <- irecv range sender tag3 commWorld
                          stat <- wait req
                          checkStatus stat sender tag3
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


scatterTest myRank = do
  numProcs <- commSize commWorld
  let segRange = (1, segmentSize)

  segment <- if myRank == zeroRank then do
    let bigRange@(low, hi) = (1, segmentSize * numProcs)
    (msg :: ArrMsg) <- newListArray bigRange [low..hi]
    sendScatter msg segRange zeroRank commWorld
    else recvScatter segRange zeroRank commWorld

  let myRankNo = fromRank myRank
      expected = take 10 [myRankNo*10+1..]

  recvMsg <- getElems segment
  recvMsg == expected @? "Rank " ++ show myRank ++ " got segment " ++ show recvMsg ++ " instead of " ++ show expected
  where
    segmentSize = 10

-- scatter list [1..] in a way such that:
-- rank 0 will receive [1]
-- rank 1 will receive [2,3]
-- rank 2 will receive [3,4,5]
-- rank 3 will receive [6,7,8,9]
-- etc
scattervTest myRank = do
  numProcs <- commSize commWorld

  let bigRange@(low, hi) = (1, sum [1..numProcs])
      recvRange = (0, myRankNo)
      myRankNo = fromRank myRank
      counts = [1..numProcs]
      displs = (0:(Prelude.init $ scanl1 (+) $ [1..numProcs]))

  segment <- if myRank == zeroRank then do
    (msg :: ArrMsg) <- newListArray bigRange [low..hi]

    let msgRange = (1, numProcs)
    (packCounts :: ArrMsg) <- newListArray msgRange $ map (sizeOf (undefined::Int) *) counts
    (packDispls :: ArrMsg) <- newListArray msgRange $ map (sizeOf (undefined::Int) *) displs

    sendScatterv msg packCounts packDispls recvRange zeroRank commWorld
    else recvScatterv recvRange zeroRank commWorld

  recvMsg <- getElems segment

  let myCount = counts!!myRankNo
      myDispl = displs!!myRankNo
      expected = take myCount $ drop myDispl [low..hi]
  recvMsg == expected @? "Rank = " ++ show myRank ++ " got segment = " ++ show recvMsg ++ " instead of " ++ show expected

gatherTest myRank = do
  numProcs <- commSize commWorld

  let segRange@(low,hi) = (1, segmentSize)
  (msg :: ArrMsg) <- newListArray segRange [low..hi]

  if myRank /= zeroRank
    then sendGather msg zeroRank commWorld
    else do
    let bigRange = (1, segmentSize * numProcs)
        expected = concat $ replicate numProcs [1..segmentSize]
    result <- recvGather msg bigRange zeroRank commWorld
    recvMsg <- getElems result
    recvMsg == expected @? "Rank " ++ show myRank ++ " got " ++ show recvMsg ++ " instead of " ++ show expected
  where segmentSize = 10

gathervTest myRank = do
  numProcs <- commSize commWorld
  let bigRange = (1, sum [1..numProcs])

  let myRankNo = fromRank myRank
      sendRange = (0, myRankNo)
  (msg :: ArrMsg) <- newListArray sendRange [0..myRankNo]
  if myRank /= zeroRank
    then sendGatherv msg zeroRank commWorld
    else do
    let msgRange = (1, numProcs)
        counts = [1..numProcs]
        displs = (0:(Prelude.init $ scanl1 (+) $ [1..numProcs]))
        expected = concat $ reverse $ take numProcs $ iterate Prelude.init [0..numProcs-1]
    (packCounts :: ArrMsg) <- newListArray msgRange $ map (sizeOf (undefined::Int) *) counts
    (packDispls :: ArrMsg) <- newListArray msgRange $ map (sizeOf (undefined::Int) *) displs

    segment <- recvGatherv msg packCounts packDispls bigRange zeroRank commWorld
    recvMsg <- getElems segment

    recvMsg == expected @? "Rank = " ++ show myRank ++ " got segment = " ++ show recvMsg ++ " instead of " ++ show expected
