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
{-
Test.Runner vs TestFramework
----------------------------
In order to be able to debug MPI bindings testsuite on a single-node
MPI installation one has to be able to separate output from processes
of different rank.

OpenMPI allows to do so via the --output-filename switch of mpirun,
but MPICH2 does not have similar feature. And since most of the output
in the testsuite is done from inside test harness library, there is
very little control over output.

Obvious solution would be to redirect stdout of the process to some
other file handle via dup2(2). However, there are several downsides:
1. Binding for dup2 (hDuplicateTo) is a GHC-only solutions
2. TestFramework does not play well with this solution, shutting
   output completely when stdout is redirected (probably "ncurses" is
   disappointed to find that output is not a terminal anymore)

Nevertheless, I decided to stick to hDuplicateTo and ditch
TestFramework in favor of TestRunner, since it allows for consistent
experience across MPI implementations.

-}
{-
Code coverage analysis
----------------------
It's very nice to have code coverage report for testsuite to make sure
that no major piece of code is left untested. However, current
profiling mechanism does not play well with MPI: when mpirun starts
two processes (on the single node), they both try to run to the same
.tix file at once. Mayhem ensues.

In order to fix this, Testsuite.hs has been made to depend on hpc
package, and after all tests has been run, HPC API is instructed to
write tix data to files rank<n>.tix.

Command line tool "hpc" could then be used to combine those into
single .tix file, which could be used to produce code coverage report.
Simple script "bin/coverage.sh" does all this automatically. Note:
script should be run from the toplevel project dir (where
haskell-mpi.cabal is residing).

-}
{-
How to set up OpenMPI on 2 (3,4,..) nodes?
------------------------------------------
Quick intro for the impatient:
1)Set up OpenMPI on each node
2)Either use a global filesystem, or make sure that binary is on each
node in the $PATH
3)If you have several network interfaces on a particular node, but
want to use only some of them, edit
/etc/openmpi/openmpi-mca-params.conf and add there:
btl_tcp_if_include=wlan0
oob_tcp_if_include=wlan0
oob_tcp_include=wlan0
4)Create hostfile
5)Use mpirun -np X --hostfile <hostfile>
-}
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
