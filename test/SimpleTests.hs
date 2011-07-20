module SimpleTests (simpleTests) where

import TestHelpers
import Control.Parallel.MPI.Simple

import Control.Concurrent (threadDelay)
import Data.Serialize ()

root :: Rank
root = 0

simpleTests :: Rank -> [(String,TestRunnerTest)]
simpleTests rank =
  [ mpiTestCase rank "send+recv simple message" $ syncSendRecv send
  , mpiTestCase rank "send+recv simple message (with sending process blocking)" syncSendRecvBlock
  , mpiTestCase rank "send+recv simple message using anySource" $ syncSendRecvAnySource send
  , mpiTestCase rank "ssend+recv simple message" $ syncSendRecv ssend
  , mpiTestCase rank "rsend+recv simple message" $ syncRSendRecv
  , mpiTestCase rank "send+recvFuture simple message" syncSendRecvFuture
  , mpiTestCase rank "isend+recv simple message" $ asyncSendRecv isend
  , mpiTestCase rank "issend+recv simple message" $ asyncSendRecv issend
  , mpiTestCase rank "isend+recv two messages"   asyncSendRecv2
  , mpiTestCase rank "isend+recvFuture two messages, out of order" asyncSendRecv2ooo
  , mpiTestCase rank "isend+recvFuture two messages (criss-cross)" crissCrossSendRecv
  , mpiTestCase rank "isend+issend+waitall two messages" waitallTest
  , mpiTestCase rank "broadcast message" broadcastTest
  , mpiTestCase rank "scatter message" scatterTest
  , mpiTestCase rank "gather message" gatherTest
  , mpiTestCase rank "allgather message" allgatherTest
  , mpiTestCase rank "alltoall message" alltoallTest
  ]
syncSendRecv, syncSendRecvAnySource  :: (Comm -> Rank -> Tag -> SmallMsg -> IO ()) -> Rank -> IO ()
asyncSendRecv :: (Comm -> Rank -> Tag -> BigMsg   -> IO Request) -> Rank -> IO ()
syncRSendRecv, syncSendRecvBlock, syncSendRecvFuture, asyncSendRecv2, asyncSendRecv2ooo :: Rank -> IO ()
crissCrossSendRecv, broadcastTest, scatterTest, gatherTest, allgatherTest, alltoallTest :: Rank -> IO ()
waitallTest :: Rank -> IO ()

-- Serializable tests
type SmallMsg = (Bool, Int, String, [()])
smallMsg :: SmallMsg
smallMsg = (True, 12, "fred", [(), (), ()])
syncSendRecv sendf rank
  | rank == sender   = sendf commWorld receiver 123 smallMsg
  | rank == receiver = do (result, status) <- recv commWorld sender 123
                          checkStatus status sender 123
                          result == smallMsg @? "Got garbled result " ++ show result
  | otherwise        = return () -- idling

syncSendRecvAnySource sendf rank
  | rank == sender   = sendf commWorld receiver 234 smallMsg
  | rank == receiver = do (result, status) <- recv commWorld anySource 234
                          checkStatus status sender 234
                          result == smallMsg @? "Got garbled result " ++ show result
  | otherwise        = return () -- idling

syncRSendRecv rank
  | rank == sender   = do threadDelay (2* 10^(6 :: Integer))
                          rsend commWorld receiver 123 smallMsg
  | rank == receiver = do (result, status) <- recv commWorld sender 123
                          checkStatus status sender 123
                          result == smallMsg @? "Got garbled result " ++ show result
  | otherwise        = return () -- idling

type BigMsg = [Int]
bigMsg :: BigMsg
bigMsg = [0..50000]
syncSendRecvBlock rank
  | rank == sender   = send commWorld receiver 456 bigMsg
  | rank == receiver = do (result, status) <- recv commWorld sender 456
                          checkStatus status sender 456
                          threadDelay (2* 10^(6 :: Integer))
                          (result::BigMsg) == bigMsg @? "Got garbled result: " ++ show (length result)
  | otherwise        = return () -- idling

syncSendRecvFuture rank
  | rank == sender   = do send commWorld receiver 789 bigMsg
  | rank == receiver = do future <- recvFuture commWorld sender 789
                          result <- waitFuture future
                          status <- getFutureStatus future
                          checkStatus status sender 789
                          (result::BigMsg) == bigMsg @? "Got garbled result: " ++ show (length result)
  | otherwise        = return () -- idling

asyncSendRecv isendf rank
  | rank == sender   = do req <- isendf commWorld receiver 123456 bigMsg
                          status <- wait req
                          checkStatusIfNotMPICH2 status sender 123456
  | rank == receiver = do (result, status) <- recv commWorld sender 123456
                          checkStatus status sender 123456
                          (result::BigMsg) == bigMsg @? "Got garbled result: " ++ show (length result)
  | otherwise        = return () -- idling

asyncSendRecv2 rank
  | rank == sender   = do req1 <- isend commWorld receiver 123 smallMsg
                          req2 <- isend commWorld receiver 456 bigMsg
                          stat1 <- wait req1
                          checkStatusIfNotMPICH2 stat1 sender 123
                          stat2 <- wait req2
                          checkStatusIfNotMPICH2 stat2 sender 456
  | rank == receiver = do (result1, stat1) <- recv commWorld sender 123
                          checkStatus stat1 sender 123
                          (result2, stat2) <- recv commWorld sender 456
                          checkStatus stat2 sender 456
                          (result2::BigMsg) == bigMsg && result1 == smallMsg @? "Got garbled result"
  | otherwise        = return () -- idling

asyncSendRecv2ooo rank
  | rank == sender   = do req1 <- isend commWorld receiver 123 smallMsg
                          req2 <- isend commWorld receiver 456 bigMsg
                          stat1 <- wait req1
                          checkStatusIfNotMPICH2 stat1 sender 123
                          stat2 <- wait req2
                          checkStatusIfNotMPICH2 stat2 sender 456
  | rank == receiver = do future2 <- recvFuture commWorld sender 456
                          future1 <- recvFuture commWorld sender 123
                          result2 <- waitFuture future2
                          result1 <- waitFuture future1
                          stat1 <- getFutureStatus future1
                          stat2 <- getFutureStatus future2
                          checkStatus stat1 sender 123
                          checkStatus stat2 sender 456
                          (length (result2::BigMsg) == length bigMsg) && (result1 == smallMsg) @? "Got garbled result"
  | otherwise        = return () -- idling

crissCrossSendRecv rank
  | rank == sender   = do req <- isend commWorld receiver 123 smallMsg
                          future <- recvFuture commWorld receiver 456
                          result <- waitFuture future
                          (length (result::BigMsg) == length bigMsg) @? "Got garbled BigMsg"
                          status <- getFutureStatus future
                          checkStatus status receiver 456
                          status2 <- wait req
                          checkStatusIfNotMPICH2 status2 sender 123
  | rank == receiver = do req <- isend commWorld sender 456 bigMsg
                          future <- recvFuture commWorld sender 123
                          result <- waitFuture future
                          (result == smallMsg) @? "Got garbled SmallMsg"
                          status <- getFutureStatus future
                          checkStatus status sender 123
                          status2 <- wait req
                          checkStatusIfNotMPICH2 status2 receiver 456
  | otherwise        = return () -- idling

waitallTest rank
  | rank == sender   = do req1 <- isend commWorld receiver 123 smallMsg
                          req2 <- isend commWorld receiver 789 smallMsg
                          [stat1, stat2] <- waitall [req1, req2]
                          checkStatusIfNotMPICH2 stat1 sender 123
                          checkStatusIfNotMPICH2 stat2 sender 789
  | rank == receiver = do (msg1,_) <- recv commWorld sender 123
                          (msg2,_) <- recv commWorld sender 789
                          msg1 == smallMsg @? "Got garbled msg1"
                          msg2 == smallMsg @? "Got garbled msg2"
  | otherwise        = return () -- idling


broadcastTest rank 
  | rank == root = bcastSend commWorld sender bigMsg
  | otherwise    = do result <- bcastRecv commWorld sender
                      (result::BigMsg) == bigMsg @? "Got garbled BigMsg"

gatherTest rank
  | rank == root = do result <- gatherRecv commWorld root [fromRank rank :: Int]
                      numProcs <- commSize commWorld
                      let expected = concat $ reverse $ take numProcs $ iterate Prelude.init [0..numProcs-1]
                          got = concat (result::[[Int]])
                      got == expected @? "Got " ++ show got ++ " instead of " ++ show expected
  | otherwise        = gatherSend commWorld root [0..fromRank rank :: Int]

scatterTest rank
  | rank == root = do numProcs <- commSize commWorld
                      result <- scatterSend commWorld root $ map (^(2::Int)) [1..numProcs]
                      result == 1 @? "Root got " ++ show result ++ " instead of 1"
  | otherwise        = do result <- scatterRecv commWorld root
                          let expected = (fromRank rank + 1::Int)^(2::Int)
                          result == expected @? "Got " ++ show result ++ " instead of " ++ show expected

allgatherTest rank = do
  let msg = [fromRank rank]
  numProcs <- commSize commWorld
  result <- allgather commWorld msg
  let expected = map (:[]) [0..numProcs-1]
  result == expected @? "Got " ++ show result ++ " instead of " ++ show expected

-- Each rank sends its own number (Int) with sendCounts [1,2,3..]
-- Each rank receives Ints with recvCounts [rank+1,rank+1,rank+1,...]
-- Rank 0 should receive 0,1,2
-- Rank 1 should receive 0,0,1,1,2,2
-- Rank 2 should receive 0,0,0,1,1,1,2,2,2
-- etc
alltoallTest myRank = do
  numProcs <- commSize commWorld
  let myRankNo = fromRank myRank
      msg = take numProcs $ map (`take` (repeat myRankNo)) [1..]
      expected = map (replicate (myRankNo+1)) (take numProcs [0..])

  result <- alltoall commWorld msg

  result == expected @? "Got " ++ show result ++ " instead of " ++ show expected
