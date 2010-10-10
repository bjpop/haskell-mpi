{-# LANGUAGE ScopedTypeVariables, ForeignFunctionInterface #-}
module StorableArrayTests (storableArrayTests) where

import TestHelpers
import Control.Parallel.MPI.Storable
import Data.Array.Storable (StorableArray, newListArray, getElems)

import Control.Concurrent (threadDelay)
import Control.Monad (when)

import Foreign
import Foreign.C.Types

root :: Rank
root = 0

storableArrayTests :: Rank -> [(String,TestRunnerTest)]
storableArrayTests rank =
  [ mpiTestCase rank "send+recv storable array"  $ syncSendRecvTest send
  , mpiTestCase rank "ssend+recv storable array" $ syncSendRecvTest ssend
  , mpiTestCase rank "rsend+recv storable array" $ rsendRecvTest
  , mpiTestCase rank "isend+irecv storable array"  $ asyncSendRecvTest isend
  , mpiTestCase rank "issend+irecv storable array" $ asyncSendRecvTest issend
  , mpiTestCase rank "broadcast storable array" broadcastTest
  , mpiTestCase rank "scatter storable array"   scatterTest
  , mpiTestCase rank "scatterv storable array"  scattervTest
  , mpiTestCase rank "gather storable array"    gatherTest
  , mpiTestCase rank "gatherv storable array"   gathervTest
  , mpiTestCase rank "allgather storable array"   allgatherTest
  , mpiTestCase rank "allgatherv storable array"   allgathervTest
  , mpiTestCase rank "alltoall storable array"   alltoallTest
  , mpiTestCase rank "alltoallv storable array"   alltoallvTest
  , mpiTestCase rank "reduce storable array"   reduceTest
  , mpiTestCase rank "allreduce storable array"   allreduceTest
  , mpiTestCase rank "reduceScatter storable array"   reduceScatterTest
  , mpiTestCase rank "reduce storable array with user-defined operation"   reduceUserOpTest
  ]
syncSendRecvTest  :: (Comm -> Rank -> Tag -> ArrMsg -> IO ()) -> Rank -> IO ()
asyncSendRecvTest :: (Comm -> Rank -> Tag -> ArrMsg -> IO Request) -> Rank -> IO ()
rsendRecvTest, broadcastTest, scatterTest, scattervTest, gatherTest, gathervTest :: Rank -> IO ()
allgatherTest, allgathervTest, alltoallTest, alltoallvTest, reduceTest, allreduceTest, reduceScatterTest :: Rank -> IO ()
reduceUserOpTest :: Rank -> IO ()

-- StorableArray tests
type ArrMsg = StorableArray Int Int

low,hi :: Int
range :: (Int, Int)
range@(low,hi) = (1,10)

arrMsg :: IO ArrMsg
arrMsg = newListArray range [low..hi]

-- Convenience shortcuts
-- sendToReceiver :: forall i e.(Ix i, Storable e, AsMpiDatatype (StorableArray i e)) => Tag -> StorableArray i e -> IO ()
-- recvFromSender :: forall i e.(Ix i, Storable e, AsMpiDatatype) => Tag -> StorableArray i e -> IO Status

syncSendRecvTest sendf rank
  | rank == sender   = do msg <- arrMsg
                          sendf commWorld receiver tag2 msg
  | rank == receiver = do (newMsg::ArrMsg, status) <- intoNewArray range $ recv commWorld sender tag2
                          checkStatus status sender tag2
                          elems <- getElems newMsg
                          elems == [low..hi::Int] @? "Got wrong array: " ++ show elems
  | otherwise        = return ()

rsendRecvTest rank = do
  when (rank == receiver) $ do (newMsg::ArrMsg, status) <- intoNewArray range $ recv commWorld sender tag2
                               checkStatus status sender tag2
                               elems <- getElems newMsg
                               elems == [low..hi::Int] @? "Got wrong array: " ++ show elems
  when (rank == sender)   $ do msg <- arrMsg
                               threadDelay (2* 10^(6 :: Integer))
                               rsend commWorld receiver tag2 msg
  return ()

asyncSendRecvTest isendf rank
  | rank == sender   = do msg <- arrMsg
                          req <- isendf commWorld receiver tag3 msg
                          stat <- wait req
                          checkStatus stat sender tag3
  -- XXX this type annotation is ugly. Is there a way to make it nicer?
  | rank == receiver = do (newMsg, req) <- intoNewArray range $ irecv commWorld sender tag3
                          stat <- wait req
                          checkStatus stat sender tag3
                          elems <- getElems newMsg
                          elems == [low..hi::Int] @? "Got wrong array: " ++ show elems
  | otherwise        = return ()

broadcastTest myRank = do
  msg <- arrMsg
  expected <- arrMsg
  if myRank == root
     then bcastSend commWorld sender (msg :: ArrMsg)
     else bcastRecv commWorld sender (msg :: ArrMsg)
  elems <- getElems msg
  expectedElems <- getElems expected
  elems == expectedElems @? "StorableArray bcast yielded garbled result: " ++ show elems


scatterTest myRank = do
  numProcs <- commSize commWorld
  let segRange = (1, segmentSize)

  ( segment :: ArrMsg) <- if myRank == root then do
        let bigRange@(low, hi) = (1, segmentSize * numProcs)
        (msg :: ArrMsg) <- newListArray bigRange [low..hi]
        intoNewArray_ segRange $ sendScatter commWorld root msg
      else intoNewArray_ segRange $ recvScatter commWorld root

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

  (segment::ArrMsg) <- if myRank == root then do
    (msg :: ArrMsg) <- newListArray bigRange [low..hi]

    let msgRange = (1, numProcs)
    (packCounts :: ArrMsg) <- newListArray msgRange counts
    (packDispls :: ArrMsg) <- newListArray msgRange displs

    intoNewArray_ recvRange $ sendScatterv commWorld root msg packCounts packDispls
    else intoNewArray_ recvRange $ recvScatterv commWorld root

  recvMsg <- getElems segment

  let myCount = counts!!myRankNo
      myDispl = displs!!myRankNo
      expected = take myCount $ drop myDispl [low..hi]
  recvMsg == expected @? "Rank = " ++ show myRank ++ " got segment = " ++ show recvMsg ++ " instead of " ++ show expected

gatherTest myRank = do
  numProcs <- commSize commWorld

  let segRange@(low,hi) = (1, segmentSize)
  (msg :: ArrMsg) <- newListArray segRange [low..hi]

  if myRank /= root
    then sendGather commWorld root msg
    else do
    let bigRange = (1, segmentSize * numProcs)
        expected = concat $ replicate numProcs [1..segmentSize]
    (result :: ArrMsg) <- intoNewArray_ bigRange $ recvGather commWorld root msg
    recvMsg <- getElems result
    recvMsg == expected @? "Rank " ++ show myRank ++ " got " ++ show recvMsg ++ " instead of " ++ show expected
  where segmentSize = 10

gathervTest myRank = do
  numProcs <- commSize commWorld
  let bigRange = (1, sum [1..numProcs])

  let myRankNo = fromRank myRank
      sendRange = (0, myRankNo)
  (msg :: ArrMsg) <- newListArray sendRange [0..myRankNo]
  if myRank /= root
    then sendGatherv commWorld root msg
    else do
    let msgRange = (1, numProcs)
        counts = [1..numProcs]
        displs = (0:(Prelude.init $ scanl1 (+) $ [1..numProcs]))
        expected = concat $ reverse $ take numProcs $ iterate Prelude.init [0..numProcs-1]
    (packCounts :: ArrMsg) <- newListArray msgRange counts
    (packDispls :: ArrMsg) <- newListArray msgRange displs

    (segment::ArrMsg) <- intoNewArray_ bigRange $ recvGatherv commWorld root msg packCounts packDispls
    recvMsg <- getElems segment

    recvMsg == expected @? "Rank = " ++ show myRank ++ " got segment = " ++ show recvMsg ++ " instead of " ++ show expected

allgatherTest _ = do
  numProcs <- commSize commWorld

  let segRange@(low,hi) = (1, segmentSize)
  (msg :: ArrMsg) <- newListArray segRange [low..hi]

  let bigRange = (1, segmentSize * numProcs)
      expected = concat $ replicate numProcs [1..segmentSize]
  (result::ArrMsg) <- intoNewArray_ bigRange $ allgather commWorld msg
  recvMsg <- getElems result
  recvMsg == expected @? "Got " ++ show recvMsg ++ " instead of " ++ show expected
  where segmentSize = 10

allgathervTest myRank = do
  numProcs <- commSize commWorld
  let bigRange = (1, sum [1..numProcs])

  let myRankNo = fromRank myRank
      sendRange = (0, myRankNo)
  (msg :: ArrMsg) <- newListArray sendRange [0..myRankNo]

  let msgRange = (1, numProcs)
      counts = [1..numProcs]
      displs = (0:(Prelude.init $ scanl1 (+) $ [1..numProcs]))
      expected = concat $ reverse $ take numProcs $ iterate Prelude.init [0..numProcs-1]
  (packCounts :: ArrMsg) <- newListArray msgRange counts
  (packDispls :: ArrMsg) <- newListArray msgRange displs

  (result::ArrMsg) <- intoNewArray_ bigRange $ allgatherv commWorld msg packCounts packDispls
  recvMsg <- getElems result

  recvMsg == expected @? "Got segment = " ++ show recvMsg ++ " instead of " ++ show expected

alltoallTest myRank = do
  numProcs <- commSize commWorld

  let myRankNo = fromRank myRank
      sendRange = (0, numProcs-1)
  (msg :: ArrMsg) <- newListArray sendRange $ take numProcs $ repeat (maxBound - myRankNo)

  let recvRange = sendRange
      expected = map (maxBound-) [0..numProcs-1]

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
      expected   = concatMap (replicate (myRankNo+1)) (take numProcs [0..])

  (packSendCounts :: ArrMsg) <- newListArray (1, length sendCounts) sendCounts
  (packSendDispls :: ArrMsg) <- newListArray (1, length sendDispls) sendDispls
  (packRecvCounts :: ArrMsg) <- newListArray (1, length recvCounts) recvCounts
  (packRecvDispls :: ArrMsg) <- newListArray (1, length recvDispls) recvDispls
  (msg :: ArrMsg) <- newListArray (1, sum sendCounts) $ take (sum sendCounts) $ repeat myRankNo

  (result::ArrMsg) <- intoNewArray_ (1, length expected) $ alltoallv commWorld msg packSendCounts packSendDispls
                                                                                   packRecvCounts packRecvDispls
  recvMsg <- getElems result

  recvMsg == expected @? "Got " ++ show recvMsg ++ " instead of " ++ show expected

-- Reducing arrays [0,1,2....] with SUM should yield [0,numProcs,2*numProcs, ...]
reduceTest myRank = do
  numProcs <- commSize commWorld
  (src :: ArrMsg) <- newListArray (0,99) [0..99]
  if myRank /= root
    then sendReduce commWorld root sumOp src
    else do
    (result :: ArrMsg) <- intoNewArray_ (0,99) $ recvReduce commWorld root sumOp src
    recvMsg <- getElems result
    let expected = map (numProcs*) [0..99]
    recvMsg == expected @? "Got " ++ show recvMsg ++ " instead of " ++ show expected

allreduceTest _ = do
  numProcs <- commSize commWorld
  (src :: ArrMsg) <- newListArray (0,99) [0..99]
  (result :: ArrMsg) <- intoNewArray_ (0,99) $ allreduce commWorld sumOp src
  recvMsg <- getElems result
  let expected = map (numProcs*) [0..99]
  recvMsg == expected @? "Got " ++ show recvMsg ++ " instead of " ++ show expected

-- We reduce [0..] with SUM.
-- Each process gets (rank+1) elements of the result
reduceScatterTest myRank = do
  numProcs <- commSize commWorld
  let dataSize = sum [1..numProcs]
      msg = take dataSize [0..]
      myRankNo = fromRank myRank
  (src :: ArrMsg) <- newListArray (1,dataSize) msg
  (counts :: StorableArray Int Int) <- newListArray (1, numProcs) [1..numProcs]
  (result :: ArrMsg) <- intoNewArray_ (1,myRankNo + 1) $ reduceScatter commWorld sumOp counts src
  recvMsg <- getElems result
  let expected = map (numProcs*) $ take (myRankNo+1) $ drop (sum [0..myRankNo]) msg
  recvMsg == expected @? "Got " ++ show recvMsg ++ " instead of " ++ show expected

-- Reducing arrays [0,1,2....] with SUM should yield [0,numProcs,2*numProcs, ...]
foreign import ccall "wrapper" 
  wrap :: (Ptr CDouble -> Ptr CDouble -> Ptr CInt -> Ptr Datatype -> IO ()) 
          -> IO (FunPtr (Ptr CDouble -> Ptr CDouble -> Ptr CInt -> Ptr Datatype -> IO ()))
reduceUserOpTest myRank = do
  numProcs <- commSize commWorld
  userSumPtr <- wrap userSum
  mySumOp <- opCreate True userSumPtr
  (src :: ArrMsg) <- newListArray (0,99) [0..99]
  if myRank /= root
    then sendReduce commWorld root sumOp src
    else do
    (result :: ArrMsg) <- intoNewArray_ (0,99) $ recvReduce commWorld root mySumOp src
    recvMsg <- getElems result
    let expected = map (numProcs*) [0..99]
    recvMsg == expected @? "Got " ++ show recvMsg ++ " instead of " ++ show expected
  freeHaskellFunPtr userSumPtr
  where
    userSum :: Ptr CDouble -> Ptr CDouble -> Ptr CInt -> Ptr Datatype -> IO ()
    userSum inPtr inoutPtr lenPtr _ = do
      len <- peek lenPtr
      let offs = sizeOf ( undefined :: CDouble )
      let loop 0 _ _ = return ()
          loop n inPtr inoutPtr = do
            a <- peek inPtr
            b <- peek inoutPtr
            poke inoutPtr (a+b)
            loop (n-1) (plusPtr inPtr offs) (plusPtr inoutPtr offs)
      loop len inPtr inoutPtr