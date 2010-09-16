module Main where

import qualified Control.Parallel.MPI.Serializable as Serializable
import TestHelpers
import StorableArrayTests

import Control.Concurrent (threadDelay)
import Control.Monad (when)
import Foreign.Storable (peek, poke)
import Foreign.Marshal (alloca)
import System.Posix.IO (dupTo, stdError, stdOutput)

import Trace.Hpc.Tix
import Trace.Hpc.Reflect


main :: IO ()
main = do
  provided <- initThread Multiple
  size <- commSize commWorld
  rank <- commRank commWorld
  if (size < 2) 
    then putStrLn $ unlines [ "Need at least two processes to run the tests."
                            , "Typical command line could look like this:"
                            , "'mpirun -np 2 bindings-mpi-testsuite 1>sender.log 2>receiver.log'" ]
    else do when (rank /= 0) $ do _ <- dupTo stdError stdOutput  -- redirect stdout to stderr for non-root processes
                                  return ()
            putStrLn $ "MPI implementation provides thread support level: " ++ show provided
            testRunnerMain $ tests rank
            barrier commWorld -- synchronize processes after all tests
            -- Dump profiling data
            tix <- examineTix
            writeTix ("rank" ++ (show rank) ++ ".tix") tix
  finalize

tests :: Rank -> [(String, TestRunnerTest)]
tests rank =
  [ testCase "Peeking/poking Status" statusPeekPoke
  ] ++ serializableTests rank
  ++ storableArrayTests rank
  
serializableTests :: Rank -> [(String,TestRunnerTest)]
serializableTests rank =
  [ mpiTestCase rank "Sending (sync)/receiving (sync) simple message" syncSendRecv
  , mpiTestCase rank "Sending (sync)/receiving (sync) simple message (with one process blocking)" syncSendRecvBlock
  , mpiTestCase rank "Sending (sync)/receiving (futures) simple message" syncSendRecvFuture
  , mpiTestCase rank "Sending (async)/receiving (sync) simple message" asyncSendRecv
  , mpiTestCase rank "Sending (async)/receiving (sync) two messages"   asyncSendRecv2
  , mpiTestCase rank "Sending (async)/receiving (futures) two messages, out of order" asyncSendRecv2ooo
  , mpiTestCase rank "Criss-cross sending/receiving (async+futures) two messages" crissCrossSendRecv
  , mpiTestCase rank "Broadcast message" broadcast
  ]
syncSendRecv, syncSendRecvBlock, syncSendRecvFuture, asyncSendRecv, asyncSendRecv2, asyncSendRecv2ooo :: Rank -> IO ()
crissCrossSendRecv, broadcast :: Rank -> IO ()


-- Serializable tests
statusPeekPoke :: IO ()
statusPeekPoke = do
  alloca $ \statusPtr -> do
    let s0 = Status minBound 2 3 maxBound 5
    poke statusPtr s0
    s1 <- peek statusPtr
    s0 == s1 @? ("Poked " ++ show s0 ++ ", but peeked " ++ show s1)

type SmallMsg = (Bool, Int, String, [()])
smallMsg :: SmallMsg 
smallMsg = (True, 12, "fred", [(), (), ()])
syncSendRecv rank 
  | rank == sender   = Serializable.send smallMsg receiver tag0 commWorld
  | rank == receiver = do (status, result) <- Serializable.recv sender tag0 commWorld
                          checkStatus status sender tag0
                          result == smallMsg @? "Got garbled result " ++ show result
  | otherwise        = return () -- idling

type BigMsg = [Int]
bigMsg :: BigMsg
bigMsg = [0..50000]
syncSendRecvBlock rank 
  | rank == sender   = Serializable.send bigMsg receiver tag1 commWorld
  | rank == receiver = do (status, result) <- Serializable.recv sender tag1 commWorld
                          checkStatus status sender tag1
                          threadDelay (2* 10^(6 :: Integer))
                          (result::BigMsg) == bigMsg @? "Got garbled result: " ++ show (length result)
  | otherwise        = return () -- idling

syncSendRecvFuture rank 
  | rank == sender   = do Serializable.send bigMsg receiver tag2 commWorld
  | rank == receiver = do future <- Serializable.recvFuture sender tag2 commWorld
                          result <- Serializable.waitFuture future
                          status <- Serializable.getFutureStatus future
                          checkStatus status sender tag2
                          (result::BigMsg) == bigMsg @? "Got garbled result: " ++ show (length result)
  | otherwise        = return () -- idling

asyncSendRecv rank 
  | rank == sender   = do req <- Serializable.isend bigMsg receiver tag3 commWorld
                          status <- wait req
                          checkStatus status sender tag3
  | rank == receiver = do (status, result) <- Serializable.recv sender tag3 commWorld
                          checkStatus status sender tag3
                          (result::BigMsg) == bigMsg @? "Got garbled result: " ++ show (length result)
  | otherwise        = return () -- idling

asyncSendRecv2 rank 
  | rank == sender   = do req1 <- Serializable.isend smallMsg receiver tag0 commWorld
                          req2 <- Serializable.isend bigMsg receiver tag1 commWorld
                          stat1 <- wait req1
                          checkStatus stat1 sender tag0
                          stat2 <- wait req2
                          checkStatus stat2 sender tag1
  | rank == receiver = do (stat1, result1) <- Serializable.recv sender tag0 commWorld
                          checkStatus stat1 sender tag0
                          (stat2, result2) <- Serializable.recv sender tag1 commWorld
                          checkStatus stat2 sender tag1
                          (result2::BigMsg) == bigMsg && result1 == smallMsg @? "Got garbled result"
  | otherwise        = return () -- idling

asyncSendRecv2ooo rank 
  | rank == sender   = do req1 <- Serializable.isend smallMsg receiver tag0 commWorld
                          req2 <- Serializable.isend bigMsg receiver tag1 commWorld
                          stat1 <- wait req1
                          checkStatus stat1 sender tag0
                          stat2 <- wait req2
                          checkStatus stat2 sender tag1
  | rank == receiver = do future2 <- Serializable.recvFuture sender tag1 commWorld
                          future1 <- Serializable.recvFuture sender tag0 commWorld
                          result2 <- Serializable.waitFuture future2
                          result1 <- Serializable.waitFuture future1
                          stat1 <- Serializable.getFutureStatus future1
                          stat2 <- Serializable.getFutureStatus future2
                          checkStatus stat1 sender tag0
                          checkStatus stat2 sender tag1
                          (length (result2::BigMsg) == length bigMsg) && (result1 == smallMsg) @? "Got garbled result"
  | otherwise        = return () -- idling

crissCrossSendRecv rank 
  | rank == sender   = do req <- Serializable.isend smallMsg receiver tag0 commWorld
                          future <- Serializable.recvFuture receiver tag1 commWorld
                          result <- Serializable.waitFuture future
                          (length (result::BigMsg) == length bigMsg) @? "Got garbled BigMsg"
                          status <- Serializable.getFutureStatus future
                          checkStatus status receiver tag1
                          status2 <- wait req
                          checkStatus status2 sender tag0
  | rank == receiver = do req <- Serializable.isend bigMsg sender tag1 commWorld
                          future <- Serializable.recvFuture sender tag0 commWorld
                          result <- Serializable.waitFuture future
                          (result == smallMsg) @? "Got garbled SmallMsg"
                          status <- Serializable.getFutureStatus future
                          checkStatus status sender tag0
                          status2 <- wait req
                          checkStatus status2 receiver tag1
  | otherwise        = return () -- idling


broadcast _ = do
  result <- Serializable.bcast bigMsg sender commWorld
  (result::BigMsg) == bigMsg @? "Got garbled BigMsg"
-- End of serializable tests