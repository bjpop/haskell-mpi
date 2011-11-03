module TestHelpers (
  module Test.Runner,
  module Test.HUnit,
  module Test.HUnit.Lang,
  mpiTestCase,
  testCase,
  checkStatus,
  checkStatusIfNotMPICH2,
  Actor(..),
  sender,
  receiver,
  ) where

import Test.Runner
import Test.HUnit ((@?), Test(..))
import Test.HUnit.Lang (Assertion)

import Control.Parallel.MPI.Base as Base

-- Test case creation helpers
mpiTestCase :: Rank -> String -> (Rank -> IO ()) -> (String,TestRunnerTest)
mpiTestCase rank title worker = 
  -- Processes are synchronized before each test with "barrier"
  testCase (unwords ["[ rank",show rank,"]",title]) $ (barrier commWorld >> worker rank)

testCase :: String -> Assertion -> (String, TestRunnerTest)
testCase title body = (title, TestRunnerTest $ TestCase body)

-- Dissect status returned by some multi-target functions
checkStatus :: Status -> Rank -> Tag -> IO ()
checkStatus _status src tag = do
  status_source _status    == src @? "Wrong source in status: expected " ++ show src ++ ", but got " ++ show (status_source _status)
  status_tag _status       == tag @? "Wrong tag in status: expected " ++ show tag ++ ", but got " ++ show (status_tag  _status)
  -- Error status is not checked since MPI implementation does not have to set it to 0 if there were no error
  -- status_error _status     == 0 @? "Non-zero error code: " ++ show (status_error _status)

-- | MPICH2 does not fill Status for non-blocking point-to-point sends, which would mark many tests as errors.
-- Hence, this kludge.
checkStatusIfNotMPICH2 :: Status -> Rank -> Tag -> IO ()
checkStatusIfNotMPICH2 status src tag =
  if getImplementation == MPICH2 
  then return ()
  else checkStatus status src tag

-- Commonly used constants
data Actor = Sender | Receiver
   deriving (Enum, Eq)

sender, receiver :: Rank
sender = toRank Sender
receiver = toRank Receiver
