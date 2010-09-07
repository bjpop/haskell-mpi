module Main where

import Test.Runner
import Test.HUnit ((@?), Test(..))
import Test.HUnit.Lang (Assertion)

import qualified Control.Parallel.MPI.Serializable as Serializable
import qualified Control.Parallel.MPI.StorableArray as Storable
import Control.Parallel.MPI.Common

import Control.Concurrent (threadDelay)
import Control.Monad (when)
import Foreign.Storable (peek, poke)
import Foreign.Marshal (alloca)
import GHC.IO.Handle  (hDuplicateTo) -- for redirecting stdout to stderr
import System.IO (stdout, stderr)
import Data.Array (rangeSize)
import Data.Array.Storable (StorableArray, newListArray, getElems, getBounds)

main :: IO ()
main = do
  provided <- initThread Multiple
  size <- commSize commWorld
  rank <- commRank commWorld
  if (size /= 2) 
    then putStrLn $ unlines [ "Need exactly two processes to run the tests."
                            , "Typical command line to achieve this is:"
                            , "'mpirun -np 2 bindings-mpi-testsuite 1>sender.log 2>receiver.log'" ]
    else do when (rank == 1) $ do hDuplicateTo stderr stdout  -- redirect stdout to stderr for receiver process
            putStrLn $ "MPI implementation provides thread support level: " ++ show provided
            testRunnerMain $ tests rank
            barrier commWorld -- synchronize processes after all tests
  finalize

tests :: Rank -> [(String, TestRunnerTest)]
tests rank =
  [ testCase "Peeking/poking Status" statusPeekPoke
  ] ++ serializableTests rank
  ++ storableTests rank
  
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

storableTests :: Rank -> [(String,TestRunnerTest)]
storableTests rank =
  [ mpiTestCase rank "Sending (sync)/receiving (sync) simple array" arraySyncRecv
  , mpiTestCase rank "Broadcast array" arrayBroadcast
  ]
arraySyncRecv, arrayBroadcast :: Rank -> IO ()

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
  | rank == sender   = do req <- Serializable.iSend bigMsg receiver tag3 commWorld
                          status <- wait req
                          checkStatus status sender tag3
  | rank == receiver = do (status, result) <- Serializable.recv sender tag3 commWorld
                          checkStatus status sender tag3
                          (result::BigMsg) == bigMsg @? "Got garbled result: " ++ show (length result)
  | otherwise        = return () -- idling

asyncSendRecv2 rank 
  | rank == sender   = do req1 <- Serializable.iSend smallMsg receiver tag0 commWorld
                          req2 <- Serializable.iSend bigMsg receiver tag1 commWorld
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
  | rank == sender   = do req1 <- Serializable.iSend smallMsg receiver tag0 commWorld
                          req2 <- Serializable.iSend bigMsg receiver tag1 commWorld
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
  | rank == sender   = do req <- Serializable.iSend smallMsg receiver tag0 commWorld
                          future <- Serializable.recvFuture receiver tag1 commWorld
                          result <- Serializable.waitFuture future
                          (length (result::BigMsg) == length bigMsg) @? "Got garbled BigMsg"
                          status <- Serializable.getFutureStatus future
                          checkStatus status receiver tag1
                          status2 <- wait req
                          checkStatus status2 sender tag0
  | rank == receiver = do req <- Serializable.iSend bigMsg sender tag1 commWorld
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

-- StorableArray tests
type ArrMsg = StorableArray Int Int

arrMsg :: IO ArrMsg
arrMsg = newListArray (0,size-1) [0..size-1]
   where
   size = 10
  
arraySyncRecv rank
  | rank == sender   = do msg <- arrMsg
                          Storable.send msg receiver tag2 commWorld
  | rank == receiver = do (status, newMsg) <- Storable.recv 10 {-TODO: nasty hardcode-} sender tag2 commWorld
                          checkStatus status sender tag2
                          elems <- getElems newMsg
                          elems == [0..9::Int] {-TODO: hardcode!-} @? "Got wrong array: " ++ show elems
  | otherwise        = return ()
  
arrayBroadcast _ = do
  msg <- arrMsg
  bs <- getBounds msg
  newMsg <- Storable.bcast (msg :: ArrMsg) (rangeSize bs) sender commWorld
  elems <- getElems msg
  newElems <- getElems newMsg
  elems == newElems @? "StorableArray bcast yielded garbled result: " ++ show newElems

-- End of StorableArray tests

-- Test case helpers
mpiTestCase :: Rank -> String -> (Rank -> IO ()) -> (String,TestRunnerTest)
mpiTestCase rank title worker = 
  -- Processes are synchronized before each test with "barrier"
  testCase (unwords ["[ rank",show rank,"]",title]) $ (barrier commWorld >> worker rank)

testCase :: String -> Assertion -> (String, TestRunnerTest)
testCase title body = (title, TestRunnerTest $ TestCase body)

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
