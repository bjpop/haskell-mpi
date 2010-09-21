module SerializableTests (serializableTests) where

import TestHelpers
import Control.Parallel.MPI.Serializable

import Control.Concurrent (threadDelay)
import Data.Serialize ()

serializableTests :: Rank -> [(String,TestRunnerTest)]
serializableTests rank =
  [ mpiTestCase rank "send+recv simple message" $ syncSendRecv send
  , mpiTestCase rank "send+recv simple message (with sending process blocking)" syncSendRecvBlock
  , mpiTestCase rank "ssend+recv simple message" $ syncSendRecv ssend
  , mpiTestCase rank "rsend+recv simple message" $ syncRSendRecv
  , mpiTestCase rank "send+recvFuture simple message" syncSendRecvFuture
  , mpiTestCase rank "isend+recv simple message" $ asyncSendRecv isend
  , mpiTestCase rank "issend+recv simple message" $ asyncSendRecv issend
  , mpiTestCase rank "isend+recv two messages"   asyncSendRecv2
  , mpiTestCase rank "isend+recvFuture two messages, out of order" asyncSendRecv2ooo
  , mpiTestCase rank "isend+recvFuture two messages (criss-cross)" crissCrossSendRecv
  , mpiTestCase rank "broadcast message" broadcast
  , mpiTestCase rank "gather message" gatherTest
  ]
syncSendRecv  :: (Comm -> Rank -> Tag -> SmallMsg -> IO ()) -> Rank -> IO ()
asyncSendRecv :: (Comm -> Rank -> Tag -> BigMsg   -> IO Request) -> Rank -> IO ()
syncRSendRecv, syncSendRecvBlock, syncSendRecvFuture, asyncSendRecv2, asyncSendRecv2ooo :: Rank -> IO ()
crissCrossSendRecv, broadcast, gatherTest :: Rank -> IO ()


-- Serializable tests
type SmallMsg = (Bool, Int, String, [()])
smallMsg :: SmallMsg 
smallMsg = (True, 12, "fred", [(), (), ()])
syncSendRecv sendf rank 
  | rank == sender   = sendf commWorld receiver tag0 smallMsg
  | rank == receiver = do (result, status) <- recv commWorld sender tag0
                          checkStatus status sender tag0
                          result == smallMsg @? "Got garbled result " ++ show result
  | otherwise        = return () -- idling

syncRSendRecv rank 
  | rank == sender   = do threadDelay (2* 10^(6 :: Integer))
                          rsend commWorld receiver tag0 smallMsg
  | rank == receiver = do (result, status) <- recv commWorld sender tag0
                          checkStatus status sender tag0
                          result == smallMsg @? "Got garbled result " ++ show result
  | otherwise        = return () -- idling

type BigMsg = [Int]
bigMsg :: BigMsg
bigMsg = [0..50000]
syncSendRecvBlock rank 
  | rank == sender   = send commWorld receiver tag1 bigMsg
  | rank == receiver = do (result, status) <- recv commWorld sender tag1
                          checkStatus status sender tag1
                          threadDelay (2* 10^(6 :: Integer))
                          (result::BigMsg) == bigMsg @? "Got garbled result: " ++ show (length result)
  | otherwise        = return () -- idling

syncSendRecvFuture rank 
  | rank == sender   = do send commWorld receiver tag2 bigMsg
  | rank == receiver = do future <- recvFuture commWorld sender tag2
                          result <- waitFuture future
                          status <- getFutureStatus future
                          checkStatus status sender tag2
                          (result::BigMsg) == bigMsg @? "Got garbled result: " ++ show (length result)
  | otherwise        = return () -- idling

asyncSendRecv isendf rank 
  | rank == sender   = do req <- isendf commWorld receiver tag3 bigMsg
                          status <- wait req
                          checkStatus status sender tag3
  | rank == receiver = do (result, status) <- recv commWorld sender tag3
                          checkStatus status sender tag3
                          (result::BigMsg) == bigMsg @? "Got garbled result: " ++ show (length result)
  | otherwise        = return () -- idling

asyncSendRecv2 rank 
  | rank == sender   = do req1 <- isend commWorld receiver tag0 smallMsg
                          req2 <- isend commWorld receiver tag1 bigMsg
                          stat1 <- wait req1
                          checkStatus stat1 sender tag0
                          stat2 <- wait req2
                          checkStatus stat2 sender tag1
  | rank == receiver = do (result1, stat1) <- recv commWorld sender tag0
                          checkStatus stat1 sender tag0
                          (result2, stat2) <- recv commWorld sender tag1
                          checkStatus stat2 sender tag1
                          (result2::BigMsg) == bigMsg && result1 == smallMsg @? "Got garbled result"
  | otherwise        = return () -- idling

asyncSendRecv2ooo rank 
  | rank == sender   = do req1 <- isend commWorld receiver tag0 smallMsg
                          req2 <- isend commWorld receiver tag1 bigMsg
                          stat1 <- wait req1
                          checkStatus stat1 sender tag0
                          stat2 <- wait req2
                          checkStatus stat2 sender tag1
  | rank == receiver = do future2 <- recvFuture commWorld sender tag1
                          future1 <- recvFuture commWorld sender tag0
                          result2 <- waitFuture future2
                          result1 <- waitFuture future1
                          stat1 <- getFutureStatus future1
                          stat2 <- getFutureStatus future2
                          checkStatus stat1 sender tag0
                          checkStatus stat2 sender tag1
                          (length (result2::BigMsg) == length bigMsg) && (result1 == smallMsg) @? "Got garbled result"
  | otherwise        = return () -- idling

crissCrossSendRecv rank 
  | rank == sender   = do req <- isend commWorld receiver tag0 smallMsg
                          future <- recvFuture commWorld receiver tag1
                          result <- waitFuture future
                          (length (result::BigMsg) == length bigMsg) @? "Got garbled BigMsg"
                          status <- getFutureStatus future
                          checkStatus status receiver tag1
                          status2 <- wait req
                          checkStatus status2 sender tag0
  | rank == receiver = do req <- isend commWorld sender tag1 bigMsg
                          future <- recvFuture commWorld sender tag0
                          result <- waitFuture future
                          (result == smallMsg) @? "Got garbled SmallMsg"
                          status <- getFutureStatus future
                          checkStatus status sender tag0
                          status2 <- wait req
                          checkStatus status2 receiver tag1
  | otherwise        = return () -- idling


broadcast _ = do
  result <- bcast commWorld sender bigMsg
  (result::BigMsg) == bigMsg @? "Got garbled BigMsg"

gatherTest rank
  | rank == zeroRank = do result <- recvGather commWorld zeroRank [fromRank rank :: Int]
                          numProcs <- commSize commWorld
                          let expected = concat $ reverse $ take numProcs $ iterate Prelude.init [0..numProcs-1]
                              got = concat (result::[[Int]])
                          got == expected @? "Got " ++ show got ++ " instead of " ++ show expected
  | otherwise        = sendGather commWorld zeroRank [0..fromRank rank :: Int]
-- End of serializable tests