module TestHelpers (
  module Test.Runner,
  module Test.HUnit,
  module Test.HUnit.Lang,
  module Control.Parallel.MPI.Common,
  mpiTestCase,
  testCase,
  checkStatus,
  Actor(..),
  sender,
  receiver,
  tag0, tag1, tag2, tag3
  ) where

import Test.Runner
import Test.HUnit ((@?), Test(..))
import Test.HUnit.Lang (Assertion)

import Control.Parallel.MPI.Common

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
  let s = fromRank src
  let t = fromTag tag
  status_source _status    == s @? "Wrong source in status: expected " ++ show s ++ ", but got " ++ show (status_source _status)
  status_tag _status       == t @? "Wrong tag in status: expected " ++ show t ++ ", but got " ++ show (status_tag  _status)
  status_cancelled _status == 0 @? "Status says cancelled: " ++ show (status_cancelled _status)
  -- Error status is not checked every time - see NOTES.txt for details
  -- status_error _status     == 0 @? "Non-zero error code: " ++ show (status_error _status)


-- Commonly used constants
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

