module SerializableTests (serializableTests) where

import TestHelpers
import Control.Parallel.MPI.Serializable

import Control.Concurrent (threadDelay)
import Data.Serialize ()

serializableTests :: Rank -> [(String,TestRunnerTest)]
serializableTests rank =
  [ mpiTestCase rank "send+recv simple message" $ syncSendRecv send
  , mpiTestCase rank "send+recv simple message (with sending process blocking)" syncSendRecvBlock
  , mpiTestCase rank "send+recv simple message" $ syncSendRecv ssend
  , mpiTestCase rank "send+recvFuture simple message" syncSendRecvFuture
  , mpiTestCase rank "isend+recv simple message" $ asyncSendRecv isend
  , mpiTestCase rank "issend+recv simple message" $ asyncSendRecv issend
  , mpiTestCase rank "isend+recv two messages"   asyncSendRecv2
  , mpiTestCase rank "Sending (async)/receiving (futures) two messages, out of order" asyncSendRecv2ooo
  , mpiTestCase rank "isend+recvFuture two messages (criss-cross)" crissCrossSendRecv
  , mpiTestCase rank "broadcast message" broadcast
  ]
syncSendRecv  :: (SmallMsg -> Rank -> Tag -> Comm -> IO ()) -> Rank -> IO ()
asyncSendRecv :: (BigMsg -> Rank -> Tag -> Comm -> IO Request) -> Rank -> IO ()
syncSendRecvBlock, syncSendRecvFuture, asyncSendRecv2, asyncSendRecv2ooo :: Rank -> IO ()
crissCrossSendRecv, broadcast :: Rank -> IO ()


-- Serializable tests
type SmallMsg = (Bool, Int, String, [()])
smallMsg :: SmallMsg 
smallMsg = (True, 12, "fred", [(), (), ()])
syncSendRecv sendf rank 
  | rank == sender   = sendf smallMsg receiver tag0 commWorld
  | rank == receiver = do (status, result) <- recv sender tag0 commWorld
                          checkStatus status sender tag0
                          result == smallMsg @? "Got garbled result " ++ show result
  | otherwise        = return () -- idling

type BigMsg = [Int]
bigMsg :: BigMsg
bigMsg = [0..50000]
syncSendRecvBlock rank 
  | rank == sender   = send bigMsg receiver tag1 commWorld
  | rank == receiver = do (status, result) <- recv sender tag1 commWorld
                          checkStatus status sender tag1
                          threadDelay (2* 10^(6 :: Integer))
                          (result::BigMsg) == bigMsg @? "Got garbled result: " ++ show (length result)
  | otherwise        = return () -- idling

syncSendRecvFuture rank 
  | rank == sender   = do send bigMsg receiver tag2 commWorld
  | rank == receiver = do future <- recvFuture sender tag2 commWorld
                          result <- waitFuture future
                          status <- getFutureStatus future
                          checkStatus status sender tag2
                          (result::BigMsg) == bigMsg @? "Got garbled result: " ++ show (length result)
  | otherwise        = return () -- idling

asyncSendRecv isendf rank 
  | rank == sender   = do req <- isendf bigMsg receiver tag3 commWorld
                          status <- wait req
                          checkStatus status sender tag3
  | rank == receiver = do (status, result) <- recv sender tag3 commWorld
                          checkStatus status sender tag3
                          (result::BigMsg) == bigMsg @? "Got garbled result: " ++ show (length result)
  | otherwise        = return () -- idling

asyncSendRecv2 rank 
  | rank == sender   = do req1 <- isend smallMsg receiver tag0 commWorld
                          req2 <- isend bigMsg receiver tag1 commWorld
                          stat1 <- wait req1
                          checkStatus stat1 sender tag0
                          stat2 <- wait req2
                          checkStatus stat2 sender tag1
  | rank == receiver = do (stat1, result1) <- recv sender tag0 commWorld
                          checkStatus stat1 sender tag0
                          (stat2, result2) <- recv sender tag1 commWorld
                          checkStatus stat2 sender tag1
                          (result2::BigMsg) == bigMsg && result1 == smallMsg @? "Got garbled result"
  | otherwise        = return () -- idling

asyncSendRecv2ooo rank 
  | rank == sender   = do req1 <- isend smallMsg receiver tag0 commWorld
                          req2 <- isend bigMsg receiver tag1 commWorld
                          stat1 <- wait req1
                          checkStatus stat1 sender tag0
                          stat2 <- wait req2
                          checkStatus stat2 sender tag1
  | rank == receiver = do future2 <- recvFuture sender tag1 commWorld
                          future1 <- recvFuture sender tag0 commWorld
                          result2 <- waitFuture future2
                          result1 <- waitFuture future1
                          stat1 <- getFutureStatus future1
                          stat2 <- getFutureStatus future2
                          checkStatus stat1 sender tag0
                          checkStatus stat2 sender tag1
                          (length (result2::BigMsg) == length bigMsg) && (result1 == smallMsg) @? "Got garbled result"
  | otherwise        = return () -- idling

crissCrossSendRecv rank 
  | rank == sender   = do req <- isend smallMsg receiver tag0 commWorld
                          future <- recvFuture receiver tag1 commWorld
                          result <- waitFuture future
                          (length (result::BigMsg) == length bigMsg) @? "Got garbled BigMsg"
                          status <- getFutureStatus future
                          checkStatus status receiver tag1
                          status2 <- wait req
                          checkStatus status2 sender tag0
  | rank == receiver = do req <- isend bigMsg sender tag1 commWorld
                          future <- recvFuture sender tag0 commWorld
                          result <- waitFuture future
                          (result == smallMsg) @? "Got garbled SmallMsg"
                          status <- getFutureStatus future
                          checkStatus status sender tag0
                          status2 <- wait req
                          checkStatus status2 receiver tag1
  | otherwise        = return () -- idling


broadcast _ = do
  result <- bcast bigMsg sender commWorld
  (result::BigMsg) == bigMsg @? "Got garbled BigMsg"
-- End of serializable tests