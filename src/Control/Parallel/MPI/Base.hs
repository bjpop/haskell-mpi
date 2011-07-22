{-# LANGUAGE ScopedTypeVariables #-}

-----------------------------------------------------------------------------
-- |
-- Module      : Control.Parallel.MPI.Base
-- Copyright   : (c) 2010 Bernie Pope, Dmitry Astapov
-- License     : BSD-style
-- Maintainer  : florbitous@gmail.com
-- Stability   : experimental
-- Portability : ghc
--
-- This module provides common MPI functionality that is independent of
-- the type of message
-- being transferred between processes. Correspondences with the C API are
-- noted in the documentation where relevant.
-----------------------------------------------------------------------------

module Control.Parallel.MPI.Base
   (
   -- * Initialization, finalization, termination.
     init
   , finalize
   , initialized
   , finalized
   , mpi
   , mpiWorld
   , initThread
   , abort

     -- * Requests and statuses.
   , Request
   , Status (..)
   , probe
   , test, testPtr
   , cancel, cancelPtr
   , wait, waitPtr
   , requestNull

   -- * Communicators and error handlers.
   , Comm
   , commWorld
   , commSelf
   , commSize
   , commRank
   , commTestInter
   , commRemoteSize
   , commCompare
   , commSetErrhandler
   , commGetErrhandler
   , commGroup
   , Errhandler
   , errorsAreFatal
   , errorsReturn

   -- * Tags.
   , Tag
   , toTag
   , fromTag
   , anyTag
   , unitTag
   , tagUpperBound

   -- Ranks.
   , Rank
   , rankId
   , toRank
   , fromRank
   , anySource
   , theRoot
   , procNull

   -- * Synchronization.
   , barrier

   -- * Groups.
   , Group
   , groupEmpty
   , groupRank
   , groupSize
   , groupUnion
   , groupIntersection
   , groupDifference
   , groupCompare
   , groupExcl
   , groupIncl
   , groupTranslateRanks

   -- * Data types.
   , Datatype
   , char
   , wchar
   , short
   , int
   , long
   , longLong
   , unsignedChar
   , unsignedShort
   , unsigned
   , unsignedLong
   , unsignedLongLong
   , float
   , double
   , longDouble
   , byte
   , packed
   , typeSize

   -- * Operators.
   , Operation
   , maxOp
   , minOp
   , sumOp
   , prodOp
   , landOp
   , bandOp
   , lorOp
   , borOp
   , lxorOp
   , bxorOp

   -- * Comparisons.
   , ComparisonResult (..)

   -- * Threads.
   , ThreadSupport (..)
   , queryThread
   , isThreadMain

   -- * Timing.
   , wtime
   , wtick
   , wtimeIsGlobal

   -- * Environment.
   , getProcessorName
   , Version (..)
   , getVersion
   , Implementation (..)
   , getImplementation

   -- * Error handling.
   , MPIError(..)
   , ErrorClass(..)
   ) where

import Prelude hiding (init)
import Control.Exception (finally)
import Control.Parallel.MPI.Internal

-- | A convenience wrapper which takes an MPI computation as its argument and wraps it
-- inside calls to 'init' (before the computation) and 'finalize' (after the computation).
-- It will make sure that 'finalize' is called even if the MPI computation raises
-- an exception (assuming the error handler is set to 'errorsThrowExceptions').
mpi :: IO () -> IO ()
mpi action = init >> (action `finally` finalize)

-- | A convenience wrapper which takes an MPI computation as its argument and wraps it
-- inside calls to 'init' (before the computation) and 'finalize' (after the computation).
-- Similar to 'mpi' but the computation is a function which is abstracted over the size of 'commWorld'
-- and the rank of the current process in 'commWorld'.
-- It will make sure that 'finalize' is called even if the MPI computation raises
-- an exception (assuming the error handler is set to 'errorsThrowExceptions').
--
-- @
-- main = mpiWorld $ \\size rank -> do
--    ...
--    ...
-- @
mpiWorld :: (Int -> Rank -> IO ()) -> IO ()
mpiWorld action = do
   init
   size <- commSize commWorld
   rank <- commRank commWorld
   action size rank `finally` finalize

-- XXX I'm temporarily leaving these comments below until we are happy with
-- the haddocks.

{- $collectives-split
Collective operations in MPI usually take a large set of arguments
that include pointers to both the input and output buffers. This fits
nicely in the C programming style, which follows this pattern:

 1. Pointers to send and receive buffers are declared.

 2. if (my_rank == root) then (send buffer is allocated and filled)

 3. Both pointers are passed to a collective function, which ignores
    the unallocated send buffer for all non-root processes.

However this style of programming is not idiomatic in Haskell.
Therefore it was decided to split most asymmetric collective calls into
two parts - sending and receiving. Thus @MPI_Gather@ is represented by
'gatherSend' and 'gatherRecv', and so on. -}

{- $arg-order
The order of arguments to most of the Haskell communication operators
is different than that of the corresponding C functions.
This was motivated by the desire to make partial application
more natural for the common case where the communicator,
rank and tag are fixed but the message varies.
-}

{- $rank-checking
Collective operations that are split into separate send/recv parts
(see above) take "root rank" as an argument. Right now no safeguards
are in place to ensure that rank supplied to the send function is
corresponding to the rank of that process. We believe that it does not
worsen the general go-on-and-shoot-yourself-in-the-foot attitide of
the MPI API.
-}

{- $err-handling
Most MPI functions may fail with an error, which, by default, will cause
the program to abort. This can be changed by setting the error
handler to 'errorsThrowExceptions'. As the name suggests, this will
turn the error into an exception which can be handled using
the facilities provided by the "Control.Exception" module.
-}

{-$example
Below is a small but complete MPI program. Process 1 sends the message
@\"Hello World\"@ to process 0. Process 0 receives the message and prints it
to standard output. It assumes that there are at least 2 MPI processes
available; a more robust program would check this condition first, before
trying to send messages.

@
module Main where

import "Control.Parallel.MPI" (mpi, commRank, commWorld, unitTag)
import "Control.Parallel.MPI.Serializable" (send, recv)
import Control.Monad (when)

main :: IO ()
main = 'mpi' $ do
   rank <- 'commRank' 'commWorld'
   when (rank == 1) $
      'send' 'commWorld' 0 'unitTag' \"Hello World\"
   when (rank == 0) $ do
      (msg, _status) <- 'recv' 'commWorld' 1 'unitTag'
      putStrLn msg
@
-}
