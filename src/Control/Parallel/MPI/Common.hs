{-# LANGUAGE ScopedTypeVariables #-}

-----------------------------------------------------------------------------
-- |
-- Module      : Control.Parallel.MPI.Common
-- Copyright   : (c) 2010 Bernie Pope, Dmitry Astapov
-- License     : BSD-style
-- Maintainer  : florbitous@gmail.com
-- Stability   : experimental
-- Portability : ghc
--
-- This module provides common MPI functionality that is independent of
-- the type of message
-- being transferred between processes. Many functions in this module bear
-- a close correspondence with those provided by the C API. Such
-- correspondences are noted in the documentation of this module where
-- relevant.
--
-- MPI is defined by the Message-Passing Interface Standard,
-- as specified by the Message Passing Interface Forum. The latest release
-- of the standard is known as MPI-2. These Haskell
-- bindings are designed to work with any MPI-2 standards compliant
-- implementation. Examples are MPICH2 and Open MPI.
--
-- In addition to reading these documents, users may also find it
-- beneficial to consult the MPI-2 standard documentation provided by the
-- MPI Forum: <http://www.mpi-forum.org>, and also the documentation for
-- the MPI implementation linked to this library (chosen when the Haskell
-- library is compiled).
--
-----------------------------------------------------------------------------

module Control.Parallel.MPI.Common
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
   , module Request
   , module Status
   , probe
   , test
   , cancel
   , wait

   -- * Communicators and error handlers.
   , module Comm
   , module Errhandler
   , commSize
   , commRank
   , commTestInter
   , commRemoteSize
   , commCompare
   , commSetErrhandler
   , commGetErrhandler
   , commGroup

   -- * Tags.
   , module Tag
   , unitTag

   -- Ranks.
   , module Rank

   -- * Synchronization.
   , barrier

   -- * Futures.
   , Future(..)
   , waitFuture
   , getFutureStatus
   , pollFuture
   , cancelFuture

   -- * Groups.
   , module Group
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
   , module Datatype
   , typeSize

   -- * Operators.
   , module Op

   -- * Comparisons.
   , module ComparisonResult

   -- * Threads.
   , module ThreadSupport
   , queryThread
   , isThreadMain

   -- * Timing.
   , wtime
   , wtick

   -- * Environment.
   , getProcessorName
   , Version (..)
   , getVersion

   ) where

import Prelude hiding (init)
import C2HS
import Control.Applicative ((<$>))
import Control.Exception (finally)
import Control.Concurrent.MVar (MVar, tryTakeMVar, readMVar)
import Control.Concurrent (ThreadId, killThread)
import qualified Control.Parallel.MPI.Internal as Internal
import Control.Parallel.MPI.Datatype as Datatype
import Control.Parallel.MPI.Comm as Comm
import Control.Parallel.MPI.Request as Request
import Control.Parallel.MPI.Status as Status
import Control.Parallel.MPI.Utils (asBool, asInt, asEnum)
import Control.Parallel.MPI.Tag as Tag
import Control.Parallel.MPI.Rank as Rank
import Control.Parallel.MPI.Group as Group
import Control.Parallel.MPI.Op as Op
import Control.Parallel.MPI.ThreadSupport as ThreadSupport
import Control.Parallel.MPI.MarshalUtils (enumToCInt, enumFromCInt)
import Control.Parallel.MPI.ComparisonResult as ComparisonResult
import Control.Parallel.MPI.Exception as Exception
import Control.Parallel.MPI.Errhandler as Errhandler

-- | A tag with unit value. Intended to be used as a convenient default.
unitTag :: Tag
unitTag = toTag ()

-- | A convenience wrapper which takes an MPI computation as its argument and wraps it
-- inside calls to 'init' (before the computation) and 'finalize' (after the computation).
-- It will make sure that 'finalize' is called even if the MPI computation raises
-- an exception. 
mpi :: IO () -> IO ()
mpi action = init >> (action `finally` finalize)

-- | A convenience wrapper (similar to 'mpi') which takes an MPI computation as its argument
-- and wraps itinside calls to 'init' (before the computation) and 'finalize' (after the computation).
-- It will make sure that 'finalize' is called even if the MPI computation raises
-- an exception. The MPI computation is a function which is abstracted over
-- the communicator size and the process rank, both with respect to 'commWorld'.
mpiWorld :: (Int -> Rank -> IO ()) -> IO ()
mpiWorld action = do
   init
   size <- commSize commWorld
   rank <- commRank commWorld
   action size rank `finally` finalize

-- | Initialize the MPI environment. The MPI environment must be intialized by each
-- MPI process before any other MPI function is called. Note that
-- the environment may also be initialized by the functions 'initThread', 'mpi',
-- and 'mpiWorld'. It is an error to attempt to initialize the environment more
-- than once for a given MPI program execution. The only MPI functions that may
-- be called before the MPI environment is initialized are 'getVersion',
-- 'initialized' and 'finalized'. Corresponds to @MPI_Init@.
init :: IO ()
init = checkError Internal.init

-- | Determine if the MPI environment has been initialized. Returns @True@ if the
-- environment has been initialized and @False@ otherwise. This function
-- may be called before the MPI environment has been initialized and after it
-- has been finalized.
-- Corresponds to @MPI_Initialized@.
initialized :: IO Bool
initialized =
   alloca $ \flagPtr -> do
      checkError $ Internal.initialized flagPtr
      peekBool flagPtr

-- | Determine if the MPI environment has been finalized. Returns @True@ if the
-- environment has been finalized and @False@ otherwise. This function
-- may be called before the MPI environment has been initialized and after it
-- has been finalized.
-- Corresponds to @MPI_Finalized@.
finalized :: IO Bool
finalized =
   alloca $ \flagPtr -> do
      checkError $ Internal.finalized flagPtr
      peekBool flagPtr

-- | Terminate the MPI execution environment.
-- Once 'finalize' is called no other MPI functions may be called except
-- 'getVersion', 'initialized' and 'finalized'. Each process must complete
-- any pending communication that it initiated before calling 'finalize'.
-- If 'finalize' returns then regular (non-MPI) computations may continue,
-- but no further MPI computation is possible. Note: the error code returned
-- by 'finalize' is not checked. Corresponds to @MPI_Finalize@.
finalize :: IO ()
-- XXX can't call checkError on finalize, because
-- checkError calls Internal.errorClass and Internal.errorString.
-- These cannot be called after finalize (at least on OpenMPI).
finalize = Internal.finalize >> return ()

-- | Initialize the MPI environment with a /required/ level of thread support.
-- See the documentation for 'init' for more information about MPI initialization.
-- The /provided/ level of thread support is returned in the result.
-- There is no guarantee that provided will be greater than or equal to required.
-- The level of provided thread support depends on the underlying MPI implementation,
-- and may also depend on information provided when the program is executed
-- (for example, by supplying appropriate arguments to mpiexec).
-- If the required level of support cannot be provided then it will try to
-- return the least supported level greater than what was required.
-- If that cannot be satisfied then it will return the highest supported level
-- provided by the MPI implementation. See the documentation for 'ThreadSupport'
-- for information about what levels are available and their relative ordering.
-- Corresponds to @MPI_Init_thread@.
initThread :: ThreadSupport -> IO ThreadSupport
initThread required = asEnum $ checkError . Internal.initThread (enumToCInt required)

queryThread :: IO Bool
queryThread = asBool $ checkError . Internal.queryThread

isThreadMain :: IO Bool
isThreadMain = asBool $ checkError . Internal.isThreadMain

getProcessorName :: IO String
getProcessorName = do
  allocaBytes (fromIntegral Internal.maxProcessorName) $ \ptr ->
    alloca $ \lenPtr -> do
       checkError $ Internal.getProcessorName ptr lenPtr
       (len :: CInt) <- peek lenPtr
       peekCStringLen (ptr, cIntConv len)

data Version =
   Version { version :: Int, subversion :: Int }
   deriving (Eq, Ord)

instance Show Version where
   show v = show (version v) ++ "." ++ show (subversion v)

getVersion :: IO Version
getVersion = do
   alloca $ \versionPtr ->
      alloca $ \subversionPtr -> do
         checkError $ Internal.getVersion versionPtr subversionPtr
         version <- peekIntConv versionPtr
         subversion <- peekIntConv subversionPtr
         return $ Version version subversion

commSize :: Comm -> IO Int
commSize comm = asInt $ checkError . Internal.commSize comm

commRank :: Comm -> IO Rank
commRank comm =
   alloca $ \ptr -> do
      checkError $ Internal.commRank comm ptr
      toRank <$> peek ptr

commTestInter :: Comm -> IO Bool
commTestInter comm = asBool $ checkError . Internal.commTestInter comm

commRemoteSize :: Comm -> IO Int
commRemoteSize comm = asInt $ checkError . Internal.commRemoteSize comm

commCompare :: Comm -> Comm -> IO ComparisonResult
commCompare comm1 comm2 = asEnum $ checkError . Internal.commCompare comm1 comm2

probe :: Rank -> Tag -> Comm -> IO Status
probe rank tag comm = do
   let cSource = fromRank rank
       cTag    = fromTag tag
   alloca $ \statusPtr -> do
      checkError $ Internal.probe cSource cTag comm $ castPtr statusPtr
      peek statusPtr

barrier :: Comm -> IO ()
barrier comm = checkError $ Internal.barrier comm

wait :: Request -> IO Status
wait request =
   alloca $ \statusPtr ->
     alloca $ \reqPtr -> do
       poke reqPtr request
       checkError $ Internal.wait reqPtr $ castPtr statusPtr
       peek statusPtr

-- Returns Nothing if the request is not complete, otherwise
-- it returns (Just status).
test :: Request -> IO (Maybe Status)
test request =
    alloca $ \statusPtr ->
       alloca $ \reqPtr ->
          alloca $ \flagPtr -> do
              poke reqPtr request
              checkError $ Internal.test reqPtr (castPtr flagPtr) (castPtr statusPtr)
              flag <- peek flagPtr
              if flag
                 then Just <$> peek statusPtr
                 else return Nothing

cancel :: Request -> IO ()
cancel request =
   alloca $ \reqPtr -> do
       poke reqPtr request
       checkError $ Internal.cancel reqPtr

wtime, wtick :: IO Double
wtime = realToFrac <$> Internal.wtime

wtick = realToFrac <$> Internal.wtick

-- Futures
data Future a =
   Future
   { futureThread :: ThreadId
   , futureStatus :: MVar Status
   , futureVal :: MVar a
   }

waitFuture :: Future a -> IO a
waitFuture = readMVar . futureVal

getFutureStatus :: Future a -> IO Status
getFutureStatus = readMVar . futureStatus

pollFuture :: Future a -> IO (Maybe a)
pollFuture = tryTakeMVar . futureVal

-- May want to stop people from waiting on Futures which are killed...
cancelFuture :: Future a -> IO ()
cancelFuture = killThread . futureThread

commGroup :: Comm -> IO Group
commGroup comm =
   alloca $ \ptr -> do
      checkError $ Internal.commGroup comm ptr
      peek ptr

groupRank :: Group -> Rank
groupRank = withGroup Internal.groupRank toRank

groupSize :: Group -> Int
groupSize = withGroup Internal.groupSize cIntConv

withGroup :: Storable a => (Group -> Ptr a -> IO CInt) -> (a -> b) -> Group -> b
withGroup prim build group =
   unsafePerformIO $
      alloca $ \ptr -> do
         checkError $ prim group ptr
         build <$> peek ptr

groupUnion :: Group -> Group -> Group
groupUnion g1 g2 = with2Groups Internal.groupUnion id g1 g2

groupIntersection :: Group -> Group -> Group
groupIntersection g1 g2 = with2Groups Internal.groupIntersection id g1 g2

groupDifference :: Group -> Group -> Group
groupDifference g1 g2 = with2Groups Internal.groupDifference id g1 g2

groupCompare :: Group -> Group -> ComparisonResult
groupCompare g1 g2 = with2Groups Internal.groupCompare enumFromCInt g1 g2

with2Groups :: Storable a => (Group -> Group -> Ptr a -> IO CInt) -> (a -> b) -> Group -> Group -> b
with2Groups prim build group1 group2 =
   unsafePerformIO $
      alloca $ \ptr -> do
         checkError $ prim group1 group2 ptr
         build <$> peek ptr

-- Technically it might make better sense to make the second argument a Set rather than a list
-- but the order is significant in the groupIncl function (the other function, not this one).
-- For the sake of keeping their types in sync, a list is used instead.
groupExcl :: Group -> [Rank] -> Group
groupExcl group ranks = groupWithRankList Internal.groupExcl group ranks

groupIncl :: Group -> [Rank] -> Group
groupIncl group ranks = groupWithRankList Internal.groupIncl group ranks

groupWithRankList :: (Group -> CInt -> Ptr CInt -> Ptr Group -> IO CInt) -> Group -> [Rank] -> Group
groupWithRankList prim group ranks =
   unsafePerformIO $ do
      let (rankIntList :: [Int]) = map fromEnum ranks
      alloca $ \groupPtr ->
         withArrayLen rankIntList $ \size ranksPtr -> do
            checkError $ prim group (enumToCInt size) (castPtr ranksPtr) groupPtr
            peek groupPtr

groupTranslateRanks :: Group -> [Rank] -> Group -> [Rank]
groupTranslateRanks group1 ranks group2 =
   unsafePerformIO $ do
      let (rankIntList :: [Int]) = map fromEnum ranks
      withArrayLen rankIntList $ \size ranksPtr ->
         allocaArray size $ \resultPtr -> do
            checkError $ Internal.groupTranslateRanks group1 (enumToCInt size) (castPtr ranksPtr) group2 resultPtr
            map toRank <$> peekArray size resultPtr

typeSize :: Datatype -> Int
typeSize dataType = unsafePerformIO $
   alloca $ \ptr -> do
      checkError $ Internal.typeSize dataType ptr
      fromIntegral <$> peek ptr

commSetErrhandler :: Comm -> Errhandler -> IO ()
commSetErrhandler comm h = checkError $ Internal.commSetErrhandler comm h

commGetErrhandler :: Comm -> IO Errhandler
commGetErrhandler comm =
   alloca $ \handlerPtr -> do
      checkError $ Internal.commGetErrhandler comm handlerPtr
      peek handlerPtr

abort :: Comm -> CInt -> IO ()
abort comm code = checkError $ Internal.abort comm code

