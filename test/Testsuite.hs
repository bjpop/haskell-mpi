module Main where
import Test.Framework (defaultMain, testGroup, Test)
import Test.Framework.Providers.HUnit (testCase)

import Test.HUnit ((@?),(@?=))

import Control.Parallel.MPI.Serializable
import Control.Concurrent (threadDelay)
import Foreign.Storable (peek, poke)
import Foreign.Marshal (alloca)

main :: IO ()
main = mpi $ do
  size <- commSize commWorld
  if (size < 2) 
    then putStrLn "Need at least two processes to run the tests"
    else do rank <- commRank commWorld
            defaultMain $ tests rank
            barrier commWorld

tests :: Rank -> [Test]
tests rank = 
  [ testCase "Peek/poke of status works" $ statusPeekPoke
  , mpiTestCase rank "Sending (sync)"  "Receiving (sync)"    "simple message + peeking Status" syncSendRecv
  , mpiTestCase rank "Sending (sync)"  "Receiving (sync)"    "simple message (with one process blocking)" syncSendRecvBlock
  , mpiTestCase rank "Sending (sync)"  "Receiving (futures)" "simple message" syncSendRecvFuture
  , mpiTestCase rank "Sending (async)" "Receiving (sync)"    "simple message" asyncSendRecv
  , mpiTestCase rank "Sending (async)" "Receiving (sync)"    "two messages"   asyncSendRecv2
  , mpiTestCase rank "Sending (async)" "Receiving (futures)" "two messages, out of order" asyncSendRecv2OOO
  ]

syncSendRecv, syncSendRecvBlock, syncSendRecvFuture, asyncSendRecv, asyncSendRecv2, asyncSendRecv2OOO :: Rank -> IO ()

statusPeekPoke = do
  alloca $ \statusPtr -> do
    let s0 = Status 1 2 3 maxBound 5
    poke statusPtr s0
    s1 <- peek statusPtr
    s0 == s1 @? ("Poked " ++ show s0 ++ ", but peeked " ++ show s1)

type SmallMsg = (Bool, Int, String, [()])
smallMsg :: SmallMsg 
smallMsg = (True, 12, "fred", [(), (), ()])
    
syncSendRecv rank 
  | rank == sender   = do send smallMsg receiver tag0 commWorld
  | rank == receiver = do (_status, result) <- recv sender tag0 commWorld
                          checkStatus _status 0 0
                          result == smallMsg @? "Got garbled result " ++ show result
  | otherwise        = return () -- idling

type BigMsg = [Int]
bigMsg :: BigMsg
bigMsg = [0..50000]
syncSendRecvBlock rank 
  | rank == sender   = do send bigMsg receiver tag1 commWorld
  | rank == receiver = do (_status, result) <- recv sender tag1 commWorld
                          checkStatus _status 0 1
                          threadDelay (2*10^6)
                          length (result::BigMsg) == length bigMsg @? "Got garbled result"
  | otherwise        = return () -- idling

syncSendRecvFuture rank 
  | rank == sender   = do send bigMsg receiver tag2 commWorld
  | rank == receiver = do future <- recvFuture sender tag2 commWorld
                          threadDelay (2*10^6)
                          result <- waitFuture future
                          _status <- getFutureStatus future
                          checkStatus _status 0 2
                          length (result::BigMsg) == length bigMsg @? "Got garbled result"
  | otherwise        = return () -- idling

asyncSendRecv rank 
  | rank == sender   = do req <- iSend bigMsg receiver tag3 commWorld
                          _status <- wait req
                          checkStatus _status 0 3
  | rank == receiver = do (_status, result) <- recv sender tag3 commWorld
                          checkStatus _status 0 3
                          length (result::BigMsg) == length bigMsg @? "Got garbled result " ++ show (length result)
  | otherwise        = return () -- idling

asyncSendRecv2 rank 
  | rank == sender   = do req1 <- iSend smallMsg receiver tag0 commWorld
                          req2 <- iSend bigMsg receiver tag1 commWorld
                          stat1 <- wait req1
                          checkStatus stat1 0 0
                          status_error stat1 == 0 @? "Errors while sending (stat1)"
                          stat2 <- wait req2
                          checkStatus stat2 0 1
                          status_error stat2 == 0 @? "Errors while sending (stat2)"
  | rank == receiver = do (stat1, result1) <- recv sender tag0 commWorld
                          checkStatus stat1 0 0
                          (stat2, result2) <- recv sender tag3 commWorld
                          checkStatus stat2 0 1
                          (length (result2::BigMsg) == length bigMsg) && (result1 == smallMsg) @? "Got garbled result"
  | otherwise        = return () -- idling

asyncSendRecv2OOO rank 
  | rank == sender   = do req1 <- iSend smallMsg receiver tag0 commWorld
                          req2 <- iSend bigMsg receiver tag1 commWorld
                          stat1 <- wait req1
                          checkStatus stat1 0 0
                          status_error stat1 == 0 @? "Errors while sending (stat1)"
                          stat2 <- wait req2
                          checkStatus stat2 0 1
                          status_error stat2 == 0 @? "Errors while sending (stat2)"
  | rank == receiver = do future2 <- recvFuture sender tag1 commWorld
                          future1 <- recvFuture sender tag0 commWorld
                          result2 <- waitFuture future2
                          result1 <- waitFuture future1
                          stat1 <- getFutureStatus future1
                          stat2 <- getFutureStatus future2
                          checkStatus stat1 0 0
                          checkStatus stat2 0 1
                          (length (result2::BigMsg) == length bigMsg) && (result1 == smallMsg) @? "Got garbled result"
  | otherwise        = return () -- idling


-- Test helpers
mpiTestGroup :: Rank -> String -> String -> String -> [Test] -> Test
mpiTestGroup rank title_sender title_receiver common body
  | rank == sender   = testGroup (unwords [title_sender, common]) body
  | rank == receiver = testGroup (unwords [title_receiver, common]) body
  | otherwise        = testGroup "Idling" []
                       
-- We want to synchronize processes after each test case, hence the use of "barrier"
mpiTestCase :: Rank -> String -> String -> String -> (Rank -> IO a) -> Test
mpiTestCase rank title_sender title_receiver common worker
  | rank == sender   = testCase (unwords [title_sender, common]) $ (worker rank   >> barrier commWorld)
  | rank == receiver = testCase (unwords [title_receiver, common]) $ (worker rank >> barrier commWorld)
  | otherwise        = testCase "Idling" $ return ()

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

checkStatus :: Status -> Int -> Int -> IO ()
checkStatus _status s t = do
  status_source _status    == s @? "Wrong source in status: expected " ++ show s ++ ", but got " ++ show (status_source _status)
  status_tag _status       == t @? "Wrong tag in status: expected " ++ show t ++ ", but got " ++ show (status_tag  _status)
  status_cancelled _status == 0 @? "Status says cancelled: " ++ show (status_cancelled _status)
  status_error _status     == 0 @? "Non-zero error code: " ++ show (status_error _status)
  