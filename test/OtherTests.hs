module OtherTests (otherTests) where

import TestHelpers

import Foreign.Storable (peek, poke)
import Foreign.Marshal (alloca)
import Foreign.C.Types (CInt)
import Control.Parallel.MPI.Base
import Data.Maybe (isJust)

otherTests :: ThreadSupport -> Rank -> [(String,TestRunnerTest)]
otherTests threadSupport _ =
   [ testCase "Peeking/poking Status" statusPeekPoke
   , testCase "Querying MPI implementation" getImplementationTest
   , testCase "Universe size" universeSizeTest
   , testCase "wtime/wtick" wtimeWtickTest
   , testCase "commGetParent is null" commGetParentNullTest
   , testCase "commRank, commSize, getProcessor name, version" rankSizeNameVersionTest
   , testCase "initialized" initializedTest
   , testCase "finalized" finalizedTest
   , testCase "tag value upper bound" tagUpperBoundTest
   , testCase "queryThread" $ queryThreadTest threadSupport
   , testCase "test requestNull" $ testRequestNull
   ]

queryThreadTest :: ThreadSupport -> IO ()
queryThreadTest threadSupport = do
   newThreadSupport <- queryThread
   threadSupport == newThreadSupport @?
      ("Result from queryThread: " ++ show newThreadSupport ++
       ", differs from result from initThread: " ++ show threadSupport)

statusPeekPoke :: IO ()
statusPeekPoke = do
  alloca $ \statusPtr -> do
    let s0 = Status (fromIntegral (maxBound::CInt)) 2 3 maxBound True
    poke statusPtr s0
    s1 <- peek statusPtr
    s0 == s1 @? ("Poked " ++ show s0 ++ ", but peeked " ++ show s1)

getImplementationTest :: IO ()
getImplementationTest = do
  putStrLn $ "Using " ++ show (getImplementation)

wtimeWtickTest :: IO ()
wtimeWtickTest = do
  t <- wtime
  tick <- wtick
  tick < t @? "Timer resolution is greater than current time"
  putStrLn $ "Current time is " ++ show t ++ ", timer resolution is " ++ show tick
  putStrLn $ "Wtime is global: " ++ show wtimeIsGlobal

universeSizeTest :: IO ()
universeSizeTest = do
  us <- universeSize commWorld
  putStrLn $ "Universe size is " ++ show us

rankSizeNameVersionTest :: IO ()
rankSizeNameVersionTest = do
  r <- commRank commWorld
  s <- commSize commWorld
  p <- getProcessorName
  v <- getVersion
  putStrLn $ "I am process " ++ show r ++ " out of " ++ show s ++ ", running on " ++ p ++ ", MPI version " ++ show v

initializedTest :: IO ()
initializedTest = do
   isInit <- initialized
   isInit == True @? "initialized return False, but was expected to return True"

finalizedTest :: IO ()
finalizedTest = do
   isFinal <- finalized
   isFinal == False @? "finalized return True, but was expected to return False"

tagUpperBoundTest :: IO ()
tagUpperBoundTest = do
  putStrLn $ "Maximum tag value is " ++ show tagUpperBound
  tagUpperBound /= (-1) @? "tagUpperBound has no value"

testRequestNull :: IO ()
testRequestNull = do
  status <- test requestNull
  isJust status @? "test requestNull does not return status"
  let (Just s) = status
  status_source s == anySource @? "status returned from (test requestNull) does not have source set to anySource"
  status_tag s == anyTag @? "status returned from (test requestNull) does not have tag set to anyTag"
  status_error s == 0 @? "status returned from (test requestNull) does not have error set to success"

commGetParentNullTest :: IO ()
commGetParentNullTest = do
  comm <- commGetParent
  comm == commNull @? "commGetParent did not return commNull, yet this is not dynamically-spawned process"
