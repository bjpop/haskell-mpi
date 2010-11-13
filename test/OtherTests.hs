module OtherTests (otherTests) where

import TestHelpers

import Foreign.Storable (peek, poke)
import Foreign.Marshal (alloca)
import Foreign.C.Types (CInt)
import Control.Parallel.MPI.Base

otherTests :: Rank -> [(String,TestRunnerTest)]
otherTests _ = [ testCase "Peeking/poking Status" statusPeekPoke
               , testCase "Querying MPI implementation" getImplementationTest
               , testCase "wtime/wtick" wtimeWtickTest
               , testCase "commRank, commSize, getProcessor name, version" rankSizeNameVersionTest
               , testCase "initialized" initializedTest
               , testCase "finalized" finalizedTest 
               , testCase "tag value upper bound" tagUpperBoundTest ]

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
  