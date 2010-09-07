module Main where

import Test.Framework (defaultMain, Test)
import Test.Framework.Providers.HUnit (testCase)
import Test.HUnit ((@?))

import Control.Parallel.MPI.Serializable
import Control.Parallel.MPI.Common

import Control.Concurrent (threadDelay)
import Control.Monad (when)
import Foreign.Storable (peek, poke)
import Foreign.Marshal (alloca)
import GHC.IO.Handle  (hDuplicateTo) -- for redirecting stdout to stderr
import System.IO (stdout, stderr)

main :: IO ()
main = do
  provided <- initThread Multiple
  size <- commSize commWorld
  rank <- commRank commWorld
  if (size /= 2) 
    then putStrLn $ unlines [ "Need exactly two processes to run the tests."
                            , "Typical command line to achieve this is:"
                            , "'mpirun -np 2 bindings-mpi-testsuite 1>sender.log 2>receiver.log'" ]
    else do when (rank == 1) $ hDuplicateTo stderr stdout  -- redirect stdout to stderr for receiver process
            putStrLn $ "MPI implementation provides thread support level: " ++ show provided
            defaultMain $ tests rank
            barrier commWorld -- synchronize processes after all tests
  finalize

tests :: Rank -> [Test]
tests rank =
  [ testCase "Peeking/poking Status" $ statusPeekPoke
  , mpiTestCase rank "Sending (sync)/receiving (sync) simple message" syncSendRecv
  , mpiTestCase rank "Sending (sync)/receiving (sync) simple message (with one process blocking)" syncSendRecvBlock
  , mpiTestCase rank "Sending (sync)/receiving (futures) simple message" syncSendRecvFuture
  , mpiTestCase rank "Sending (async)/receiving (sync) simple message" asyncSendRecv
  , mpiTestCase rank "Sending (async)/receiving (sync) two messages"   asyncSendRecv2
  , mpiTestCase rank "Sending (async)/receiving (futures) two messages, out of order" asyncSendRecv2ooo
  ]

syncSendRecv, syncSendRecvBlock, syncSendRecvFuture, asyncSendRecv, asyncSendRecv2, asyncSendRecv2ooo :: Rank -> IO ()

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
  | rank == sender   = send smallMsg receiver tag0 commWorld
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
                          length (result::BigMsg) == length bigMsg @? "Got garbled result: " ++ show (length result)
  | otherwise        = return () -- idling

syncSendRecvFuture rank 
  | rank == sender   = do send bigMsg receiver tag2 commWorld
  | rank == receiver = do future <- recvFuture sender tag2 commWorld
                          result <- waitFuture future
                          status <- getFutureStatus future
                          checkStatus status sender tag2
                          length (result::BigMsg) == length bigMsg @? "Got garbled result: " ++ show (length result)
  | otherwise        = return () -- idling

asyncSendRecv rank 
  | rank == sender   = do req <- iSend bigMsg receiver tag3 commWorld
                          status <- wait req
                          checkStatus status sender tag3
  | rank == receiver = do (status, result) <- recv sender tag3 commWorld
                          checkStatus status sender tag3
                          length (result::BigMsg) == length bigMsg @? "Got garbled result: " ++ show (length result)
  | otherwise        = return () -- idling

asyncSendRecv2 rank 
  | rank == sender   = do req1 <- iSend smallMsg receiver tag0 commWorld
                          req2 <- iSend bigMsg receiver tag1 commWorld
                          stat1 <- wait req1
                          checkStatus stat1 sender tag0
                          stat2 <- wait req2
                          checkStatus stat2 sender tag1
  | rank == receiver = do (stat1, result1) <- recv sender tag0 commWorld
                          checkStatus stat1 sender tag0
                          (stat2, result2) <- recv sender tag1 commWorld
                          checkStatus stat2 sender tag1
                          (length (result2::BigMsg) == length bigMsg) && (result1 == smallMsg) @? "Got garbled result"
  | otherwise        = return () -- idling

asyncSendRecv2ooo rank 
  | rank == sender   = do req1 <- iSend smallMsg receiver tag0 commWorld
                          req2 <- iSend bigMsg receiver tag1 commWorld
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


-- Test case helpers
mpiTestCase :: Rank -> String -> (Rank -> IO ()) -> Test
mpiTestCase rank title worker = 
  -- Processes are synchronized before each test with "barrier"
  testCase (unwords ["[rank",show rank,"]",title]) $ (barrier commWorld >> worker rank)

data Actor = Sender | Receiver
   deriving (Enum, Eq)

sender, receiver :: Rank
sender = toRank Sender
receiver = toRank Receiver

tag0, tag1, tag2, tag3 :: Tag
tag0 = toTag (0 :: Int)
tag1 = toTag (1 :: Int)
tag2 = toTag (2 :: Int)
tag3 = toTag (3 :: Int)

checkStatus :: Status -> Rank -> Tag -> IO ()
checkStatus _status src tag = do
  let s = fromRank src
  let t = fromTag tag
  status_source _status    == s @? "Wrong source in status: expected " ++ show s ++ ", but got " ++ show (status_source _status)
  status_tag _status       == t @? "Wrong tag in status: expected " ++ show t ++ ", but got " ++ show (status_tag  _status)
  status_cancelled _status == 0 @? "Status says cancelled: " ++ show (status_cancelled _status)
  -- Error status is not checked every time - see NOTES.txt for details
  -- status_error _status     == 0 @? "Non-zero error code: " ++ show (status_error _status)
