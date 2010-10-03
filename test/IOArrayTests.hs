{-# LANGUAGE ScopedTypeVariables #-}
module IOArrayTests (ioArrayTests) where

import TestHelpers
import Data.Array.Storable (StorableArray)
import Control.Parallel.MPI.Storable
import Data.Array.IO (IOArray, newListArray, getElems)

import Control.Concurrent (threadDelay)
import Control.Monad (when)

ioArrayTests :: Rank -> [(String,TestRunnerTest)]
ioArrayTests rank =
  [ mpiTestCase rank "send+recv IO array"  $ syncSendRecvTest send
  , mpiTestCase rank "ssend+recv IO array" $ syncSendRecvTest ssend
  , mpiTestCase rank "rsend+recv IO array" $ rsendRecvTest
-- irecv only works for StorableArray at the moment. See comments in source.
--  , mpiTestCase rank "isend+irecv IO array"  $ asyncSendRecvTest isend
--  , mpiTestCase rank "issend+irecv IO array" $ asyncSendRecvTest issend
  , mpiTestCase rank "broadcast IO array" broadcastTest
  , mpiTestCase rank "scatter IO array"   scatterTest
  , mpiTestCase rank "scatterv IO array"  scattervTest
  , mpiTestCase rank "gather IO array"    gatherTest
  , mpiTestCase rank "gatherv IO array"   gathervTest
  , mpiTestCase rank "allgather IO array"   allgatherTest
  , mpiTestCase rank "allgatherv IO array"   allgathervTest
  , mpiTestCase rank "alltoall IO array"   alltoallTest
  , mpiTestCase rank "alltoallv IO array"   alltoallvTest
  , mpiTestCase rank "reduce IO array"   reduceTest
  ]
syncSendRecvTest  :: (Comm -> Rank -> Tag -> ArrMsg -> IO ()) -> Rank -> IO ()
-- asyncSendRecvTest :: (Comm -> Rank -> Tag -> IOArray Int Int -> IO Request) -> Rank -> IO ()
rsendRecvTest, broadcastTest, scatterTest, scattervTest, gatherTest, gathervTest :: Rank -> IO ()
allgatherTest, allgathervTest, alltoallTest, alltoallvTest, reduceTest :: Rank -> IO ()

type ElementType = Double
type ArrMsg = IOArray Int ElementType

low,hi :: Int
range :: (Int, Int)
range@(low,hi) = (1,10)

arrMsgContent :: [ElementType]
arrMsgContent = map fromIntegral [low..hi]

arrMsg :: IO ArrMsg
arrMsg = newListArray range arrMsgContent


syncSendRecvTest sendf rank
  | rank == sender   = do msg <- arrMsg
                          sendf commWorld receiver tag2 msg
  | rank == receiver = do (newMsg::ArrMsg, status) <- intoNewArray range $ recv commWorld sender tag2
                          checkStatus status sender tag2
                          elems <- getElems newMsg
                          elems == arrMsgContent @? "Got wrong array: " ++ show elems
  | otherwise        = return ()

rsendRecvTest rank = do
  when (rank == receiver) $ do (newMsg::ArrMsg, status) <- intoNewArray range $ recv commWorld sender tag2
                               checkStatus status sender tag2
                               elems <- getElems newMsg
                               elems == arrMsgContent @? "Got wrong array: " ++ show elems
  when (rank == sender)   $ do msg <- arrMsg
                               threadDelay (2* 10^(6 :: Integer))
                               rsend commWorld receiver tag2 msg
  return ()

{-
asyncSendRecvTest isendf rank
  | rank == sender   = do msg <- arrMsg
                          req <- isendf commWorld receiver tag3 msg
                          stat <- wait req
                          checkStatus stat sender tag3
  -- XXX this type annotation is ugly. Is there a way to make it nicer?
  | rank == receiver = do (newMsg, req) <- intoNewArray range $ (irecv commWorld sender tag3 :: IOArray Int Int -> IO Request)
                          stat <- wait req
                          checkStatus stat sender tag3
                          elems <- getElems newMsg
                          elems == [low..hi::Int] @? "Got wrong array: " ++ show elems
  | otherwise        = return ()
-}

broadcastTest myRank = do
  msg <- arrMsg
  expected <- arrMsg
  if myRank == zeroRank
     then bcastSend commWorld sender (msg :: ArrMsg)
     else bcastRecv commWorld sender (msg :: ArrMsg)
  elems <- getElems msg
  expectedElems <- getElems expected
  elems == expectedElems @? "IOArray bcast yielded garbled result: " ++ show elems


scatterTest myRank = do
  numProcs <- commSize commWorld
  let segRange = (1, segmentSize)

  (segment::ArrMsg) <- if myRank == zeroRank then do
               let bigRange@(low, hi) = (1, segmentSize * numProcs)
               (msg :: ArrMsg) <- newListArray bigRange $ map fromIntegral [low..hi]
               intoNewArray_ segRange $ sendScatter commWorld zeroRank msg
             else intoNewArray_ segRange $ recvScatter commWorld zeroRank

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

  (segment::ArrMsg) <- if myRank == zeroRank then do
    (msg :: ArrMsg) <- newListArray bigRange $ map fromIntegral [low..hi]

    let msgRange = (1, numProcs)
    (packCounts :: StorableArray Int Int) <- newListArray msgRange counts
    (packDispls :: StorableArray Int Int) <- newListArray msgRange displs

    intoNewArray_ recvRange $ sendScatterv commWorld zeroRank msg packCounts packDispls
    else intoNewArray_ recvRange $ recvScatterv commWorld zeroRank

  recvMsg <- getElems segment

  let myCount = counts!!myRankNo
      myDispl = displs!!myRankNo
      expected = map fromIntegral $ take myCount $ drop myDispl [low..hi]
  recvMsg == expected @? "Rank = " ++ show myRank ++ " got segment = " ++ show recvMsg ++ " instead of " ++ show expected

gatherTest myRank = do
  numProcs <- commSize commWorld

  let segRange@(low,hi) = (1, segmentSize)
  (msg :: ArrMsg) <- newListArray segRange $ map fromIntegral [low..hi]

  if myRank /= zeroRank
    then sendGather commWorld zeroRank msg
    else do
    let bigRange = (1, segmentSize * numProcs)
        expected = map fromIntegral $ concat $ replicate numProcs [1..segmentSize]
    (result::ArrMsg) <- intoNewArray_ bigRange $ recvGather commWorld zeroRank msg
    recvMsg <- getElems result
    recvMsg == expected @? "Rank " ++ show myRank ++ " got " ++ show recvMsg ++ " instead of " ++ show expected
  where segmentSize = 10

gathervTest myRank = do
  numProcs <- commSize commWorld
  let bigRange = (1, sum [1..numProcs])

  let myRankNo = fromRank myRank
      sendRange = (0, myRankNo)
  (msg :: ArrMsg) <- newListArray sendRange $ map fromIntegral [0..myRankNo]
  if myRank /= zeroRank
    then sendGatherv commWorld zeroRank msg
    else do
    let msgRange = (1, numProcs)
        counts = [1..numProcs]
        displs = (0:(Prelude.init $ scanl1 (+) $ [1..numProcs]))
        expected = map fromIntegral $ concat $ reverse $ take numProcs $ iterate Prelude.init [0..numProcs-1]
    (packCounts :: StorableArray Int Int) <- newListArray msgRange counts
    (packDispls :: StorableArray Int Int) <- newListArray msgRange displs

    (segment::ArrMsg) <- intoNewArray_ bigRange $ recvGatherv commWorld zeroRank msg packCounts packDispls
    recvMsg <- getElems segment

    recvMsg == expected @? "Rank = " ++ show myRank ++ " got segment = " ++ show recvMsg ++ " instead of " ++ show expected

allgatherTest _ = do
  numProcs <- commSize commWorld

  let segRange@(low,hi) = (1, segmentSize)
  (msg :: ArrMsg) <- newListArray segRange $ map fromIntegral [low..hi]

  let bigRange = (1, segmentSize * numProcs)
      expected = map fromIntegral $ concat $ replicate numProcs [1..segmentSize]
  (result::ArrMsg) <- intoNewArray_ bigRange $ allgather commWorld msg
  recvMsg <- getElems result
  recvMsg == expected @? "Got " ++ show recvMsg ++ " instead of " ++ show expected
  where segmentSize = 10

allgathervTest myRank = do
  numProcs <- commSize commWorld
  let bigRange = (1, sum [1..numProcs])

  let myRankNo = fromRank myRank
      sendRange = (0, myRankNo)
  (msg :: ArrMsg) <- newListArray sendRange $ map fromIntegral [0..myRankNo]

  let msgRange = (1, numProcs)
      counts = [1..numProcs]
      displs = (0:(Prelude.init $ scanl1 (+) $ [1..numProcs]))
      expected = map fromIntegral $ concat $ reverse $ take numProcs $ iterate Prelude.init [0..numProcs-1]
  (packCounts :: StorableArray Int Int) <- newListArray msgRange counts
  (packDispls :: StorableArray Int Int) <- newListArray msgRange displs

  (result::ArrMsg) <- intoNewArray_ bigRange $ allgatherv commWorld msg packCounts packDispls
  recvMsg <- getElems result

  recvMsg == expected @? "Got segment = " ++ show recvMsg ++ " instead of " ++ show expected

alltoallTest myRank = do
  numProcs <- commSize commWorld

  let myRankNo = fromRank myRank
      sendRange = (0, numProcs-1)
  (msg :: ArrMsg) <- newListArray sendRange $ take numProcs $ repeat myRankNo

  let recvRange = sendRange
      expected = map fromIntegral $ [0..numProcs-1]

  (result::ArrMsg) <- intoNewArray_ recvRange $ alltoall commWorld msg 1
  recvMsg <- getElems result

  recvMsg == expected @? "Got segment = " ++ show recvMsg ++ " instead of " ++ show expected

-- Each rank sends its own number (Int) with sendCounts [1,2,3..]
-- Each rank receives Ints with recvCounts [rank+1,rank+1,rank+1,...]
-- Rank 0 should receive 0,1,2
-- Rank 1 should receive 0,0,1,1,2,2
-- Rank 2 should receive 0,0,0,1,1,1,2,2,2
-- etc
alltoallvTest myRank = do
  numProcs <- commSize commWorld
  let myRankNo   = fromRank myRank
      sendCounts = take numProcs [1..]
      sendDispls = Prelude.init $ scanl1 (+) $ 0:sendCounts
      recvCounts = take numProcs (repeat (myRankNo+1))
      recvDispls = Prelude.init $ scanl1 (+) $ 0:recvCounts
      expected   = map fromIntegral $ concatMap (replicate (myRankNo+1)) (take numProcs [(0::Int)..])

  (packSendCounts :: StorableArray Int Int) <- newListArray (1, length sendCounts) sendCounts
  (packSendDispls :: StorableArray Int Int) <- newListArray (1, length sendDispls) sendDispls
  (packRecvCounts :: StorableArray Int Int) <- newListArray (1, length recvCounts) recvCounts
  (packRecvDispls :: StorableArray Int Int) <- newListArray (1, length recvDispls) recvDispls
  (msg :: ArrMsg) <- newListArray (1, sum sendCounts) $ map fromIntegral $ take (sum sendCounts) $ repeat myRankNo

  (result::ArrMsg) <- intoNewArray_ (1, length expected) $ alltoallv commWorld msg packSendCounts packSendDispls
                                                                                   packRecvCounts packRecvDispls
  recvMsg <- getElems result

  recvMsg == expected @? "Got " ++ show recvMsg ++ " instead of " ++ show expected

-- Reducing arrays [0,1,2....] with SUM should yield [0,numProcs,2*numProcs, ...]
reduceTest myRank = do
  numProcs <- commSize commWorld
  (src :: ArrMsg) <- newListArray (0,99) [0..99]
  if myRank /= zeroRank
    then sendReduce commWorld zeroRank sumOp src
    else do
    (result :: ArrMsg) <- intoNewArray_ (0,99) $ recvReduce commWorld zeroRank sumOp src
    recvMsg <- getElems result
    let expected = map ((fromIntegral numProcs)*) [0..99::ElementType]
    recvMsg == expected @? "Got " ++ show recvMsg ++ " instead of " ++ show expected
