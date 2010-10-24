module Main where

import TestHelpers
import OtherTests
import SerializableTests
import StorableArrayTests
import IOArrayTests
import GroupTests
import PrimTypeTests
import ExceptionTests

import Control.Monad (when)
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
   otherTests rank
   ++ primTypeTests rank
   ++ serializableTests rank
   ++ storableArrayTests rank
   ++ ioArrayTests rank
   ++ groupTests rank
   ++ exceptionTests rank
