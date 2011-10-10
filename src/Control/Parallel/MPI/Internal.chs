{-# LANGUAGE ForeignFunctionInterface, DeriveDataTypeable, GeneralizedNewtypeDeriving, ScopedTypeVariables #-}

{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
#include <mpi.h>
#include "init_wrapper.h"
#include "comparison_result.h"
#include "error_classes.h"
#include "thread_support.h"
-----------------------------------------------------------------------------
{- |
Module      : Control.Parallel.MPI.Internal
Copyright   : (c) 2010 Bernie Pope, Dmitry Astapov
License     : BSD-style
Maintainer  : florbitous@gmail.com
Stability   : experimental
Portability : ghc

This module contains low-level Haskell bindings to core MPI functions.
All Haskell functions correspond to MPI functions or values with the similar
name (i.e. @commRank@ is the binding for @MPI_Comm_rank@ etc)

Note that most of this module is re-exported by
"Control.Parallel.MPI", so if you are not interested in writing
low-level code, you should probably import "Control.Parallel.MPI" and
either "Control.Parallel.MPI.Storable" or "Control.Parallel.MPI.Serializable".
-}
-----------------------------------------------------------------------------
module Control.Parallel.MPI.Internal
   (

     -- * MPI runtime management.
     -- ** Initialization, finalization, termination.
     init, finalize, initialized, finalized, abort,
     -- ** Multi-threaded environment support.
     ThreadSupport (..), initThread, queryThread, isThreadMain,

     -- ** Runtime attributes.
     getProcessorName, Version (..), getVersion, Implementation(..), getImplementation, universeSize,

     -- ** Info objects
     Info, infoNull,

     -- * Requests and statuses.
     Request, Status (..), probe, test, testPtr, cancel, cancelPtr, wait, waitPtr, waitall, requestNull,

     -- * Process management.
     -- ** Communicators.
     Comm, commWorld, commSelf, commNull, commTestInter,
     commSize, commRemoteSize, 
     commRank, 
     commCompare, commGroup, commGetAttr,

     -- ** Process groups.
     Group, groupEmpty, groupRank, groupSize, groupUnion,
     groupIntersection, groupDifference, groupCompare, groupExcl,
     groupIncl, groupTranslateRanks,
     -- ** Comparisons.
     ComparisonResult (..),

     -- ** Dynamic process management
     commGetParent, commSpawn, commSpawnSimple,

     -- * Error handling.
     Errhandler, errorsAreFatal, errorsReturn, errorsThrowExceptions, commSetErrhandler, commGetErrhandler,
     ErrorClass (..), MPIError(..), mpiUndefined,

     -- * Ranks.
     Rank, rankId, toRank, fromRank, anySource, theRoot, procNull,

     -- * Data types.
     Datatype, char, wchar, short, int, long, longLong, unsignedChar, unsignedShort, unsigned, unsignedLong, unsignedLongLong, float, double, longDouble, byte, packed, typeSize,

     -- * Point-to-point operations.
     -- ** Tags.
     Tag, toTag, fromTag, anyTag, unitTag, tagUpperBound,

     -- ** Blocking operations.
     BufferPtr, Count, -- XXX: what will break if we don't export those?
     send, ssend, rsend, recv,
     -- ** Non-blocking operations.
     isend, issend, irecv,
     isendPtr, issendPtr, irecvPtr,


     -- * Collective operations.
     -- ** One-to-all.
     bcast, scatter, scatterv,
     -- ** All-to-one.
     gather, gatherv, reduce,
     -- ** All-to-all.
     allgather, allgatherv,
     alltoall, alltoallv,
     allreduce, 
     reduceScatterBlock,
     reduceScatter,
     barrier,

     -- ** Reduction operations.
     Operation, maxOp, minOp, sumOp, prodOp, landOp, bandOp, lorOp, borOp, lxorOp, bxorOp,
     opCreate, opFree,

     -- * Timing.
     wtime, wtick, wtimeIsGlobal, wtimeIsGlobalKey

   ) where

import Prelude hiding (init)
import C2HS
import Data.Typeable
import Data.Maybe (fromMaybe)
import Control.Monad (liftM, unless)
import Control.Applicative ((<$>), (<*>))
import Control.Exception

{# context prefix = "MPI" #}

-- | Pointer to memory buffer that either holds data to be sent or is
--   used to receive some data. You would
--   probably have to use 'castPtr' to pass some actual pointers to
--   API functions.
type BufferPtr = Ptr ()

-- | Count of elements in the send/receive buffer
type Count = CInt

{- |
Haskell enum that contains MPI constants
@MPI_IDENT@, @MPI_CONGRUENT@, @MPI_SIMILAR@ and @MPI_UNEQUAL@.

Those are used to compare communicators ('commCompare') and
process groups ('groupCompare'). Refer to those
functions for description of comparison rules.
-}
{# enum ComparisonResult {underscoreToCase} deriving (Eq,Ord,Show) #}

-- Which Haskell type will be used as Comm depends on the MPI
-- implementation that was selected during compilation. It could be
-- CInt, Ptr (), Ptr CInt or something else.
type MPIComm = {# type MPI_Comm #}

{- | Abstract type representing MPI communicator handle. Different MPI
   implementations use different C types to implement this, so
   concrete Haskell type behind @Comm@ is hidden from user.

   In any MPI program you have predefined communicator 'commWorld'
   which includes all running processes. You could create new
   communicators with TODO
-}
newtype Comm = MkComm { fromComm :: MPIComm } deriving Eq
peekComm ptr = MkComm <$> peek ptr

foreign import ccall "&mpi_comm_world" commWorld_ :: Ptr MPIComm
foreign import ccall "&mpi_comm_self" commSelf_ :: Ptr MPIComm

-- | Predefined handle for communicator that includes all running
-- processes. Similar to @MPI_Comm_world@
commWorld :: Comm
commWorld = MkComm <$> unsafePerformIO $ peek commWorld_

-- | Predefined handle for communicator that includes only current
-- process. Similar to @MPI_Comm_self@
commSelf :: Comm
commSelf = MkComm <$> unsafePerformIO $ peek commSelf_

foreign import ccall "&mpi_max_processor_name" max_processor_name_ :: Ptr CInt
foreign import ccall "&mpi_max_error_string" max_error_string_ :: Ptr CInt

-- | Max length of "processor name" as returned by 'getProcessorName'
maxProcessorName :: CInt
maxProcessorName = unsafePerformIO $ peek max_processor_name_

-- | Max length of error description as returned by 'errorString'
maxErrorString :: CInt
maxErrorString = unsafePerformIO $ peek max_error_string_

-- | Initialize the MPI environment. The MPI environment must be intialized by each
-- MPI process before any other MPI function is called. Note that
-- the environment may also be initialized by the functions 'initThread', 'mpi',
-- and 'mpiWorld'. It is an error to attempt to initialize the environment more
-- than once for a given MPI program execution. The only MPI functions that may
-- be called before the MPI environment is initialized are 'getVersion',
-- 'initialized' and 'finalized'. This function corresponds to @MPI_Init@.
{# fun unsafe init_wrapper as init {} -> `()' checkError*- #}

-- | Determine if the MPI environment has been initialized. Returns @True@ if the
-- environment has been initialized and @False@ otherwise. This function
-- may be called before the MPI environment has been initialized and after it
-- has been finalized.
-- This function corresponds to @MPI_Initialized@.
{# fun unsafe Initialized as ^ {alloca- `Bool' peekBool*} -> `()' checkError*- #}

-- | Determine if the MPI environment has been finalized. Returns @True@ if the
-- environment has been finalized and @False@ otherwise. This function
-- may be called before the MPI environment has been initialized and after it
-- has been finalized.
-- This function corresponds to @MPI_Finalized@.
{# fun unsafe Finalized as ^ {alloca- `Bool' peekBool*} -> `()' checkError*- #}

-- | Initialize the MPI environment with a /required/ level of thread support.
-- See the documentation for 'init' for more information about MPI initialization.
-- The /provided/ level of thread support is returned in the result.
-- There is no guarantee that provided will be greater than or equal to required.
-- The level of provided thread support depends on the underlying MPI implementation,
-- and may also depend on information provided when the program is executed
-- (for example, by supplying appropriate arguments to @mpiexec@).
-- If the required level of support cannot be provided then it will try to
-- return the least supported level greater than what was required.
-- If that cannot be satisfied then it will return the highest supported level
-- provided by the MPI implementation. See the documentation for 'ThreadSupport'
-- for information about what levels are available and their relative ordering.
-- This function corresponds to @MPI_Init_thread@.
{# fun unsafe init_wrapper_thread as initThread
                {cFromEnum `ThreadSupport', alloca- `ThreadSupport' peekEnum* } -> `()' checkError*- #}

-- | Returns the current provided level of thread support. This will be the value
-- returned as \"provided level of support\" by 'initThread' as well. This function
-- corresponds to @MPI_Query_thread@.
{# fun unsafe Query_thread as ^ {alloca- `ThreadSupport' peekEnum* } -> `()' checkError*- #}

-- | This function can be called by a thread to find out whether it is the main thread (the
-- thread that called 'init' or 'initThread'.
{# fun unsafe Is_thread_main as ^
                 {alloca- `Bool' peekBool* } -> `()' checkError*- #}

-- | Terminate the MPI execution environment.
-- Once 'finalize' is called no other MPI functions may be called except
-- 'getVersion', 'initialized' and 'finalized', however non-MPI computations
-- may continue. Each process must complete
-- any pending communication that it initiated before calling 'finalize'.
--  Note: the error code returned
-- by 'finalize' is not checked. This function corresponds to @MPI_Finalize@.
{# fun unsafe Finalize as ^ {} -> `()' discard*- #}
discard _ = return ()
-- XXX can't call checkError on finalize, because
-- checkError calls Internal.errorClass and Internal.errorString.
-- These cannot be called after finalize (at least on OpenMPI).

-- | Return the name of the current processing host. From this value it
-- must be possible to identify a specific piece of hardware on which
-- the code is running.
getProcessorName :: IO String
getProcessorName = do
  allocaBytes (fromIntegral maxProcessorName) $ \ptr -> do
    len <- getProcessorName' ptr
    peekCStringLen (ptr, cIntConv len)
  where
    getProcessorName' = {# fun unsafe Get_processor_name as getProcessorName_
                           {id `Ptr CChar', alloca- `CInt' peekIntConv*} -> `()' checkError*- #}

-- | MPI implementation version
data Version =
   Version { version :: Int, subversion :: Int }
   deriving (Eq, Ord)

instance Show Version where
   show v = show (version v) ++ "." ++ show (subversion v)

-- | Which MPI version the code is running on.
getVersion :: IO Version
getVersion = do
   (version, subversion) <- getVersion'
   return $ Version version subversion
  where
    getVersion' = {# fun unsafe Get_version as getVersion_
                     {alloca- `Int' peekIntConv*, alloca- `Int' peekIntConv*} -> `()' checkError*- #}

-- | Supported MPI implementations
data Implementation = MPICH2 | OpenMPI deriving (Eq,Show)

-- | Which MPI implementation was used during linking
getImplementation :: Implementation
getImplementation =
#ifdef MPICH2
       MPICH2
#else
       OpenMPI
#endif

-- | Return the number of processes involved in a communicator. For 'commWorld'
-- it returns the total number of processes available. If the communicator is
-- and intra-communicator it returns the number of processes in the local group.
-- This function corresponds to @MPI_Comm_size@.
{# fun unsafe Comm_size as ^
              {fromComm `Comm', alloca- `Int' peekIntConv* } -> `()' checkError*- #}

-- | For intercommunicators, returns size of the remote process group.
--   Corresponds to @MPI_Comm_remote_size@.
{# fun unsafe Comm_remote_size as ^
                    {fromComm `Comm', alloca- `Int' peekIntConv* } -> `()' checkError*- #}

{- | Check whether the given communicator is intercommunicator - that
   is, communicator connecting two different groups of processes.

Refer to MPI Report v2.2, Section 5.2 "Communicator Argument" for
more details.
-}
{# fun unsafe Comm_test_inter as ^
                   {fromComm `Comm', alloca- `Bool' peekBool* } -> `()' checkError*- #}

-- | Look up MPI communicator argument by the given numeric key.
--   Lookup of some standard MPI arguments is provided by convenience
--   functions 'tagUpperBound' and 'wtimeIsGlobal'.
commGetAttr :: Storable e => Comm -> Int -> IO (Maybe e)
commGetAttr comm key = do
  isInitialized <- initialized
  if isInitialized then do
    alloca $ \ptr -> do
      found <- commGetAttr' comm key (castPtr ptr)
      if found then do ptr2 <- peek ptr
                       Just <$> peek ptr2
               else return Nothing
    else return Nothing
      where
        commGetAttr' = {# fun unsafe Comm_get_attr as commGetAttr_
                         {fromComm `Comm', cIntConv `Int', id `Ptr ()', alloca- `Bool' peekBool*} -> `()' checkError*- #}

-- | Maximum tag value supported by the current MPI implementation. Corresponds to the value of standard MPI
--   attribute @MPI_TAG_UB@.
--
-- When called before 'init' or 'initThread' would return 0.
tagUpperBound :: Int
tagUpperBound =
  let key = unsafePerformIO (peek tagUB_)
      in fromMaybe 0 $ unsafePerformIO (commGetAttr commWorld key)

foreign import ccall unsafe "&mpi_tag_ub" tagUB_ :: Ptr Int

{- | True if clocks at all processes in
'commWorld' are synchronized, False otherwise. The expectation is that
the variation in time, as measured by calls to 'wtime', will be less then one half the
round-trip time for an MPI message of length zero. 

Communicators other than 'commWorld' could have different clocks.
You could find it out by querying attribute 'wtimeIsGlobalKey' with 'commGetAttr'.

When wtimeIsGlobal is called before 'init' or 'initThread' it would return False.
-}
wtimeIsGlobal :: Bool
wtimeIsGlobal =
  fromMaybe False $ unsafePerformIO (commGetAttr commWorld wtimeIsGlobalKey)

foreign import ccall unsafe "&mpi_wtime_is_global" wtimeIsGlobal_ :: Ptr Int

-- | Numeric key for standard MPI communicator attribute @MPI_WTIME_IS_GLOBAL@.
-- To be used with 'commGetAttr'.
wtimeIsGlobalKey :: Int
wtimeIsGlobalKey = unsafePerformIO (peek wtimeIsGlobal_)

{- | 
Many ``dynamic'' MPI applications are expected to exist in a static runtime environment, in which resources have been allocated before the application is run. When a user (or possibly a batch system) runs one of these quasi-static applications, she will usually specify a number of processes to start and a total number of processes that are expected. An application simply needs to know how many slots there are, i.e., how many processes it should spawn.

This attribute indicates the total number of processes that are expected.

When universeSize is called before 'init' or 'initThread' it would return False.
-}
universeSize :: Comm -> IO (Maybe Int)
universeSize c =
  commGetAttr c universeSizeKey

foreign import ccall unsafe "&mpi_universe_size" universeSize_ :: Ptr Int

-- | Numeric key for recommended MPI communicator attribute @MPI_UNIVERSE_SIZE@.
-- To be used with 'commGetAttr'.
universeSizeKey :: Int
universeSizeKey = unsafePerformIO (peek universeSize_)

-- | Return the rank of the calling process for the given
-- communicator. If it is an intercommunicator, returns rank of the
-- process in the local group.
{# fun unsafe Comm_rank as ^
              {fromComm `Comm', alloca- `Rank' peekIntConv* } -> `()' checkError*- #}

{- | Compares two communicators.

* If they are handles for the same MPI communicator object, result is 'Identical';

* If both communicators are identical in constituents and rank
    order, result is `Congruent';

* If they have the same members, but with different ranks, then
    result is 'Similar';

* Otherwise, result is 'Unequal'.

-}
{# fun unsafe Comm_compare as ^
                 {fromComm `Comm', fromComm `Comm', alloca- `ComparisonResult' peekEnum*} -> `()' checkError*- #}

-- | Test for an incomming message, without actually receiving it.
-- If a message has been sent from @Rank@ to the current process with @Tag@ on the
-- communicator @Comm@ then 'probe' will return the 'Status' of the message. Otherwise
-- it will block the current process until such a matching message is sent.
-- This allows the current process to check for an incoming message and decide
-- how to receive it, based on the information in the 'Status'.
-- This function corresponds to @MPI_Probe@.
{# fun Probe as ^
           {fromRank `Rank', fromTag `Tag', fromComm `Comm', allocaCast- `Status' peekCast*} -> `()' checkError*- #}
{- probe :: Rank       -- ^ Rank of the sender.
      -> Tag        -- ^ Tag of the sent message.
      -> Comm       -- ^ Communicator.
      -> IO Status  -- ^ Information about the incoming message (but not the content of the message). -}

{-| Send the values (as specified by @BufferPtr@, @Count@, @Datatype@) to
    the process specified by (@Comm@, @Rank@, @Tag@). Caller will
    block until data is copied from the send buffer by the MPI
-}
{# fun unsafe Send as ^
          { id `BufferPtr', id `Count', fromDatatype `Datatype', fromRank `Rank', fromTag `Tag', fromComm `Comm' } -> `()' checkError*- #}
{-| Variant of 'send' that would terminate only when receiving side
actually starts receiving data. 
-}
{# fun unsafe Ssend as ^
          { id `BufferPtr', id `Count', fromDatatype `Datatype', fromRank `Rank', fromTag `Tag', fromComm `Comm' } -> `()' checkError*- #}
{-| Variant of 'send' that expects the relevant 'recv' to be already
started, otherwise this call could terminate with MPI error.
-}
{# fun unsafe Rsend as ^
          { id `BufferPtr', id `Count', fromDatatype `Datatype', fromRank `Rank', fromTag `Tag', fromComm `Comm' } -> `()' checkError*- #}
-- | Receives data from the process
--   specified by (@Comm@, @Rank@, @Tag@) and stores it into buffer specified
--   by (@BufferPtr@, @Count@, @Datatype@).
{# fun unsafe Recv as ^
          { id `BufferPtr', id `Count', fromDatatype `Datatype', fromRank `Rank', fromTag `Tag', fromComm `Comm', allocaCast- `Status' peekCast* } -> `()' checkError*- #}
-- | Send the values (as specified by @BufferPtr@, @Count@, @Datatype@) to
--   the process specified by (@Comm@, @Rank@, @Tag@) in non-blocking mode.
-- 
-- Use 'probe' or 'test' to check the status of the operation,
-- 'cancel' to terminate it or 'wait' to block until it completes.
-- Operation would be considered complete as soon as MPI finishes
-- copying the data from the send buffer. 
{# fun unsafe Isend as ^
           { id `BufferPtr', id `Count', fromDatatype `Datatype', fromRank `Rank', fromTag `Tag', fromComm `Comm', alloca- `Request' peekRequest*} -> `()' checkError*- #}
-- | Variant of the 'isend' that would be considered complete only when
--   receiving side actually starts receiving data. 
{# fun unsafe Issend as ^
           { id `BufferPtr', id `Count', fromDatatype `Datatype', fromRank `Rank', fromTag `Tag', fromComm `Comm', alloca- `Request' peekRequest*} -> `()' checkError*- #}
-- | Non-blocking variant of 'recv'. Receives data from the process
--   specified by (@Comm@, @Rank@, @Tag@) and stores it into buffer specified
--   by (@BufferPtr@, @Count@, @Datatype@).
{# fun Irecv as ^
           { id `BufferPtr', id `Count', fromDatatype `Datatype', fromRank `Rank', fromTag `Tag', fromComm `Comm', alloca- `Request' peekRequest*} -> `()' checkError*- #}

-- | Like 'isend', but stores @Request@ at the supplied pointer. Useful
-- for making arrays of @Requests@ that could be passed to 'waitall'
{# fun unsafe Isend as isendPtr
           { id `BufferPtr', id `Count', fromDatatype `Datatype', fromRank `Rank', fromTag `Tag', fromComm `Comm', castPtr `Ptr Request'} -> `()' checkError*- #}

-- | Like 'issend', but stores @Request@ at the supplied pointer. Useful
-- for making arrays of @Requests@ that could be passed to 'waitall'
{# fun unsafe Issend as issendPtr
           { id `BufferPtr', id `Count', fromDatatype `Datatype', fromRank `Rank', fromTag `Tag', fromComm `Comm', castPtr `Ptr Request'} -> `()' checkError*- #}

-- | Like 'irecv', but stores @Request@ at the supplied pointer. Useful
-- for making arrays of @Requests@ that could be passed to 'waitall'
{# fun Irecv as irecvPtr
           { id `BufferPtr', id `Count', fromDatatype `Datatype', fromRank `Rank', fromTag `Tag', fromComm `Comm', castPtr `Ptr Request'} -> `()' checkError*- #}

-- | Broadcast data from one member to all members of the communicator.
{# fun unsafe Bcast as ^
           { id `BufferPtr', id `Count', fromDatatype `Datatype', fromRank `Rank', fromComm `Comm'} -> `()' checkError*- #}

-- | Blocks until all processes on the communicator call this function.
-- This function corresponds to @MPI_Barrier@.
{# fun unsafe Barrier as ^ {fromComm `Comm'} -> `()' checkError*- #}

-- | Blocking test for the completion of a send of receive.
-- See 'test' for a non-blocking variant.
-- This function corresponds to @MPI_Wait@. Request pointer could
-- be changed to point to @requestNull@. See @wait@ for variant that does not mutate request value.
{# fun unsafe Wait as waitPtr
          {castPtr `Ptr Request', allocaCast- `Status' peekCast*} -> `()' checkError*-  #}

-- | Same as @waitPtr@, but does not change Haskell @Request@ value to point to @procNull@.
-- Usually, this is harmless - your request just would be considered inactive.
wait request = withRequest request waitPtr

-- | Takes pointer to the array of Requests of given size, 'wait's on all of them,
--   populates array of Statuses of the same size. This function corresponds to @MPI_Waitall@
{# fun unsafe Waitall as ^
            { id `Count', castPtr `Ptr Request', castPtr `Ptr Status'} -> `()' checkError*- #}
-- TODO: Make this Storable Array instead of Ptr ?

-- | Non-blocking test for the completion of a send or receive.
-- Returns @Nothing@ if the request is not complete, otherwise
-- it returns @Just status@.
--
-- Note that while MPI would modify
-- request to be @requestNull@ if the operation is complete,
-- Haskell value would not be changed. So, if you got (Just status)
-- as a result, consider your request to be @requestNull@. Or use @testPtr@.
--
-- See 'wait' for a blocking variant.
-- This function corresponds to @MPI_Test@.
test :: Request -> IO (Maybe Status)
test request = withRequest request testPtr

-- | Analogous to 'test' but uses pointer to @Request@. If request is completed, pointer would be 
-- set to point to @requestNull@.
testPtr :: Ptr Request -> IO (Maybe Status)
testPtr reqPtr = do
  (flag, status) <- testPtr' reqPtr
  request' <- peek reqPtr
  if flag
    then do if request' == requestNull
               then return $ Just status
               else error "testPtr: request modified, but not set to MPI_REQUEST_NULL!"
    else return Nothing
  where testPtr' = {# fun unsafe Test as testPtr_
       {castPtr `Ptr Request', alloca- `Bool' peekBool*, allocaCast- `Status' peekCast*} -> `()' checkError*- #}

-- | Cancel a pending communication request.
-- This function corresponds to @MPI_Cancel@. Sets pointer to point to @requestNull@.
{# fun unsafe Cancel as cancelPtr
            {castPtr `Ptr Request'} -> `()' checkError*- #}


-- | Same as @cancelPtr@, but does not change Haskell @Request@ value to point to @procNull@.
-- Usually, this is harmless - your request just would be considered inactive.
cancel request = withRequest request cancelPtr

withRequest req f = do alloca $ \ptr -> do poke ptr req
                                           f (castPtr ptr)

-- | Scatter data from one member to all members of
-- a group.
{# fun unsafe Scatter as ^
   { id `BufferPtr', id `Count', fromDatatype `Datatype',
     id `BufferPtr', id `Count', fromDatatype `Datatype',
     fromRank `Rank', fromComm `Comm'} -> `()' checkError*- #}

-- | Gather data from all members of a group to one
-- member.
{# fun unsafe Gather as ^
   { id `BufferPtr', id `Count', fromDatatype `Datatype',
     id `BufferPtr', id `Count', fromDatatype `Datatype',
     fromRank `Rank', fromComm `Comm'} -> `()' checkError*- #}

-- Note: We pass counts/displs as Ptr CInt so that caller could supply nullPtr here
-- which would be impossible if we marshal arrays ourselves here.

-- | A variation of 'scatter' which allows to use data segments of
--   different length.
{# fun unsafe Scatterv as ^
   { id `BufferPtr', id `Ptr CInt', id `Ptr CInt', fromDatatype `Datatype',
     id `BufferPtr', id `Count', fromDatatype `Datatype',
     fromRank `Rank', fromComm `Comm'} -> `()' checkError*- #}

-- | A variation of 'gather' which allows to use data segments of
--   different length.
{# fun unsafe Gatherv as ^
   { id `BufferPtr', id `Count', fromDatatype `Datatype',
     id `BufferPtr', id `Ptr CInt', id `Ptr CInt', fromDatatype `Datatype',
     fromRank `Rank', fromComm `Comm'} -> `()' checkError*- #}

-- | A variation of 'gather' where all members of
-- a group receive the result.
{# fun unsafe Allgather as ^
   { id `BufferPtr', id `Count', fromDatatype `Datatype',
     id `BufferPtr', id `Count', fromDatatype `Datatype',
     fromComm `Comm'} -> `()' checkError*- #}

-- | A variation of 'allgather' that allows to use data segments of
--   different length.
{# fun unsafe Allgatherv as ^
   { id `BufferPtr', id `Count', fromDatatype `Datatype',
     id `BufferPtr', id `Ptr CInt', id `Ptr CInt', fromDatatype `Datatype',
     fromComm `Comm'} -> `()' checkError*- #}

-- | Scatter/Gather data from all
-- members to all members of a group (also called complete exchange)
{# fun unsafe Alltoall as ^
   { id `BufferPtr', id `Count', fromDatatype `Datatype',
     id `BufferPtr', id `Count', fromDatatype `Datatype',
     fromComm `Comm'} -> `()' checkError*- #}

-- | A variant of 'alltoall' allows to use data segments of different length.
{# fun unsafe Alltoallv as ^
   { id `BufferPtr', id `Ptr CInt', id `Ptr CInt', fromDatatype `Datatype',
     id `BufferPtr', id `Ptr CInt', id `Ptr CInt', fromDatatype `Datatype',
     fromComm `Comm'} -> `()' checkError*- #}

-- Reduce, allreduce and reduceScatter could call back to Haskell
-- via user-defined ops, so they should be imported in "safe" mode

-- | Applies predefined or user-defined reduction operations to data,
--   and delivers result to the single process.
{# fun Reduce as ^
   { id `BufferPtr', id `BufferPtr', id `Count', fromDatatype `Datatype',
     fromOperation `Operation', fromRank `Rank', fromComm `Comm'} -> `()' checkError*- #}

-- | Applies predefined or user-defined reduction operations to data,
--   and delivers result to all members of the group.
{# fun Allreduce as ^
   { id `BufferPtr', id `BufferPtr', id `Count', fromDatatype `Datatype',
     fromOperation `Operation', fromComm `Comm'} -> `()' checkError*- #}

-- | A combined reduction and scatter operation - result is split and
--   parts are distributed among the participating processes.
--
-- See 'reduceScatter' for variant that allows to specify personal
-- block size for each process.
--
-- Note that this call is not supported with some MPI implementations,
-- like OpenMPI <= 1.5 and would cause a run-time 'error' in that case.
#if 0
{# fun Reduce_scatter_block as ^
   { id `BufferPtr', id `BufferPtr', id `Count', fromDatatype `Datatype',
     fromOperation `Operation', fromComm `Comm'} -> `()' checkError*- #}
#else
reduceScatterBlock :: BufferPtr -> BufferPtr -> Count -> Datatype -> Operation -> Comm -> IO ()
reduceScatterBlock = error "reduceScatterBlock is not supported by OpenMPI"
#endif

-- | A combined reduction and scatter operation - result is split and
--   parts are distributed among the participating processes.
{# fun Reduce_scatter as ^
   { id `BufferPtr', id `BufferPtr', id `Ptr CInt', fromDatatype `Datatype',
     fromOperation `Operation', fromComm `Comm'} -> `()' checkError*- #}

-- TODO: In the following haddock block, mention SCAN and EXSCAN once
-- they are implemented 

{- | Binds a user-dened reduction operation to an 'Operation' handle that can
subsequently be used in 'reduce', 'allreduce', and 'reduceScatter'.
The user-defined operation is assumed to be associative. 

If second argument to @opCreate@ is @True@, then the operation should be both commutative and associative. If
it is not commutative, then the order of operands is fixed and is defined to be in ascending,
process rank order, beginning with process zero. The order of evaluation can be changed,
taking advantage of the associativity of the operation. If operation
is commutative then the order
of evaluation can be changed, taking advantage of commutativity and
associativity.

User-defined operation accepts four arguments, @invec@, @inoutvec@,
@len@ and @datatype@:

[@invec@] first input vector

[@inoutvec@] second input vector, which is also the output vector

[@len@] length of both vectors

[@datatype@] type of the elements in both vectors.

Function is expected to apply reduction operation to the elements
of @invec@ and @inoutvec@ in pariwise manner:

@
inoutvec[i] = op invec[i] inoutvec[i]
@

Full example with user-defined function that mimics standard operation
'sumOp':

@
import "Control.Parallel.MPI.Fast"

foreign import ccall \"wrapper\" 
  wrap :: (Ptr CDouble -> Ptr CDouble -> Ptr CInt -> Ptr Datatype -> IO ()) 
          -> IO (FunPtr (Ptr CDouble -> Ptr CDouble -> Ptr CInt -> Ptr Datatype -> IO ()))
reduceUserOpTest myRank = do
  numProcs <- commSize commWorld
  userSumPtr <- wrap userSum
  mySumOp <- opCreate True userSumPtr
  (src :: StorableArray Int Double) <- newListArray (0,99) [0..99]
  if myRank /= root
    then reduceSend commWorld root sumOp src
    else do
    (result :: StorableArray Int Double) <- intoNewArray_ (0,99) $ reduceRecv commWorld root mySumOp src
    recvMsg <- getElems result
  freeHaskellFunPtr userSumPtr
  where
    userSum :: Ptr CDouble -> Ptr CDouble -> Ptr CInt -> Ptr Datatype -> IO ()
    userSum inPtr inoutPtr lenPtr _ = do
      len <- peek lenPtr
      let offs = sizeOf ( undefined :: CDouble )
      let loop 0 _ _ = return ()
          loop n inPtr inoutPtr = do
            a <- peek inPtr
            b <- peek inoutPtr
            poke inoutPtr (a+b)
            loop (n-1) (plusPtr inPtr offs) (plusPtr inoutPtr offs)
      loop len inPtr inoutPtr
@
-}
{# fun unsafe Op_create as ^
   {castFunPtr `FunPtr (Ptr t -> Ptr t -> Ptr CInt -> Ptr Datatype -> IO ())', cFromEnum `Bool', alloca- `Operation' peekOperation*} -> `()' checkError*- #}

{- | Free the handle for user-defined reduction operation created by 'opCreate'
-}
{# fun Op_free as ^ {withOperation* `Operation'} -> `()' checkError*- #}

{- | Returns a 
floating-point number of seconds, representing elapsed wallclock
time since some time in the past.

The \"time in the past\" is guaranteed not to change during the life of the process.
The user is responsible for converting large numbers of seconds to other units if they are
preferred. The time is local to the node that calls @wtime@, but see 'wtimeIsGlobal'.
-}
{# fun unsafe Wtime as ^ {} -> `Double' realToFrac #}

{- | Returns the resolution of 'wtime' in seconds. That is, it returns,
as a double precision value, the number of seconds between successive clock ticks. For
example, if the clock is implemented by the hardware as a counter that is incremented
every millisecond, the value returned by @wtick@ should be 10^(-3).
-}
{# fun unsafe Wtick as ^ {} -> `Double' realToFrac #}

-- | Return the process group from a communicator. With
--   intercommunicator, returns the local group.
{# fun unsafe Comm_group as ^
               {fromComm `Comm', alloca- `Group' peekGroup*} -> `()' checkError*- #}

-- | Returns the rank of the calling process in the given group. This function corresponds to @MPI_Group_rank@.
groupRank :: Group -> Rank
groupRank = unsafePerformIO <$> groupRank'
  where groupRank' = {# fun unsafe Group_rank as groupRank_
                        {fromGroup `Group', alloca- `Rank' peekIntConv*} -> `()' checkError*- #}

-- | Returns the size of a group. This function corresponds to @MPI_Group_size@.
groupSize :: Group -> Int
groupSize = unsafePerformIO <$> groupSize'
  where groupSize' = {# fun unsafe Group_size as groupSize_
                        {fromGroup `Group', alloca- `Int' peekIntConv*} -> `()' checkError*- #}

-- | Constructs the union of two groups: all the members of the first group, followed by all the members of the 
-- second group that do not appear in the first group. This function corresponds to @MPI_Group_union@.
groupUnion :: Group -> Group -> Group
groupUnion g1 g2 = unsafePerformIO $ groupUnion' g1 g2
  where groupUnion' = {# fun unsafe Group_union as groupUnion_
                         {fromGroup `Group', fromGroup `Group', alloca- `Group' peekGroup*} -> `()' checkError*- #}

-- | Constructs a new group which is the intersection of two groups. This function corresponds to @MPI_Group_intersection@.
groupIntersection :: Group -> Group -> Group
groupIntersection g1 g2 = unsafePerformIO $ groupIntersection' g1 g2
  where groupIntersection' = {# fun unsafe Group_intersection as groupIntersection_
                                {fromGroup `Group', fromGroup `Group', alloca- `Group' peekGroup*} -> `()' checkError*- #}

-- | Constructs a new group which contains all the elements of the first group which are not in the second group. 
-- This function corresponds to @MPI_Group_difference@.
groupDifference :: Group -> Group -> Group
groupDifference g1 g2 = unsafePerformIO $ groupDifference' g1 g2
  where groupDifference' = {# fun unsafe Group_difference as groupDifference_
                              {fromGroup `Group', fromGroup `Group', alloca- `Group' peekGroup*} -> `()' checkError*- #}

-- | Compares two groups. Returns 'MPI_IDENT' if the order and members of the two groups are the same,
-- 'MPI_SIMILAR' if only the members are the same, and 'MPI_UNEQUAL' otherwise.
groupCompare :: Group -> Group -> ComparisonResult
groupCompare g1 g2 = unsafePerformIO $ groupCompare' g1 g2
  where
    groupCompare' = {# fun unsafe Group_compare as groupCompare_
                       {fromGroup `Group', fromGroup `Group', alloca- `ComparisonResult' peekEnum*} -> `()' checkError*- #}

-- Technically it might make better sense to make the second argument a Set rather than a list
-- but the order is significant in the groupIncl function (the other function, not this one).
-- For the sake of keeping their types in sync, a list is used instead.
{- | Create a new @Group@ from the given one. Exclude processes
with given @Rank@s from the new @Group@. Processes in new @Group@ will
have ranks @[0...]@.
-}
{# fun unsafe Group_excl as ^
               {fromGroup `Group', withRanksAsInts* `[Rank]'&, alloca- `Group' peekGroup*} -> `()' checkError*- #}
{- | Create a new @Group@ from the given one. Include only processes
with given @Rank@s in the new @Group@. Processes in new @Group@ will
have ranks @[0...]@.
-}
{# fun unsafe Group_incl as ^
               {fromGroup `Group', withRanksAsInts* `[Rank]'&, alloca- `Group' peekGroup*} -> `()' checkError*- #}

{- | Given two @Group@s and list of @Rank@s of some processes in the
first @Group@, return @Rank@s of those processes in the second
@Group@. If there are no corresponding @Rank@ in the second @Group@,
'mpiUndefined' is returned.

This function is important for determining the relative numbering of the same processes
in two different groups. For instance, if one knows the ranks of certain processes in the group
of 'commWorld', one might want to know their ranks in a subset of that group.
Note that 'procNull' is a valid rank for input to @groupTranslateRanks@, which
returns 'procNull' as the translated rank.
-}
groupTranslateRanks :: Group -> [Rank] -> Group -> [Rank]
groupTranslateRanks group1 ranks group2 =
   unsafePerformIO $ do
      let (rankIntList :: [Int]) = map fromEnum ranks
      withArrayLen rankIntList $ \size ranksPtr ->
         allocaArray size $ \resultPtr -> do
            groupTranslateRanks' group1 (cFromEnum size) (castPtr ranksPtr) group2 resultPtr
            map toRank <$> peekArray size resultPtr
  where
    groupTranslateRanks' = {# fun unsafe Group_translate_ranks as groupTranslateRanks_
                              {fromGroup `Group', id `CInt', id `Ptr CInt', fromGroup `Group', id `Ptr CInt'} -> `()' checkError*- #}

withRanksAsInts ranks f = withArrayLen (map fromEnum ranks) $ \size ptr -> f (cIntConv size, castPtr ptr)

{- | If a process was started with 'commSpawn', @commGetParent@
returns the parent intercommunicator of the current process. This
parent intercommunicator is created implicitly inside of 'init' and
is the same intercommunicator returned by 'commSpawn' in the
parents. If the process was not spawned, @commGetParent@ returns
'commNull'. After the parent communicator is freed or disconnected,
@commGetParent@ returns 'commNull'. -} 

{# fun unsafe Comm_get_parent as ^
               {alloca- `Comm' peekComm*} -> `()' checkError*- #}

withT = with
{# fun unsafe Comm_spawn as ^
               { `String' 
               , withT* `Ptr CChar'
               , id `Count'
               , fromInfo `Info'
               , fromRank `Rank'
               , fromComm `Comm'
               , alloca- `Comm' peekComm*
               , id `Ptr CInt'} -> `()' checkError*- #}

foreign import ccall "mpi_argv_null" mpiArgvNull_ :: Ptr CChar
foreign import ccall "mpi_errcodes_ignore" mpiErrcodesIgnore_ :: Ptr CInt
{- Simplified version of `commSpawn' that does not support argument passing and spawn error code checking -}
commSpawnSimple rank program maxprocs =
  commSpawn program mpiArgvNull_ maxprocs infoNull rank commSelf mpiErrcodesIgnore_

foreign import ccall "mpi_undefined" mpiUndefined_ :: Ptr Int

-- | Predefined constant that might be returned as @Rank@ by calls
--  like 'groupTranslateRanks'. Corresponds to @MPI_UNDEFINED@. Please
--  refer to \"MPI Report Constant And Predefined Handle Index\" for a
--  list of situations where @mpiUndefined@ could appear.
mpiUndefined :: Int
mpiUndefined = unsafePerformIO $ peek mpiUndefined_

-- | Return the number of bytes used to store an MPI @Datatype@.
typeSize :: Datatype -> Int
typeSize = unsafePerformIO . typeSize'
  where
    typeSize' =
      {# fun unsafe Type_size as typeSize_
         {fromDatatype `Datatype', alloca- `Int' peekIntConv*} -> `()' checkError*- #}

{# fun unsafe Error_class as ^
                { id `CInt', alloca- `CInt' peek*} -> `CInt' id #}

-- | Set the error handler for a communicator.
-- This function corresponds to @MPI_Comm_set_errhandler@.
{# fun unsafe Comm_set_errhandler as ^
                       {fromComm `Comm', fromErrhandler `Errhandler'} -> `()' checkError*- #}

-- | Get the error handler for a communicator.
-- This function corresponds to @MPI_Comm_get_errhandler@.
{# fun unsafe Comm_get_errhandler as ^
                       {fromComm `Comm', alloca- `Errhandler' peekErrhandler*} -> `()' checkError*- #}

-- | Tries to terminate all MPI processes in its communicator argument.
-- The second argument is an error code which /may/ be used as the return status
-- of the MPI process, but this is not guaranteed. On systems where 'Int' has a larger
-- range than 'CInt', the error code will be clipped to fit into the range of 'CInt'.
-- This function corresponds to @MPI_Abort@.
abort :: Comm -> Int -> IO ()
abort comm code =
   abort' comm (toErrorCode code)
   where
   toErrorCode :: Int -> CInt
   toErrorCode i
      -- Assumes Int always has range at least as big as CInt.
      | i < (fromIntegral (minBound :: CInt)) = minBound
      | i > (fromIntegral (maxBound :: CInt)) = maxBound
      | otherwise = cIntConv i

   abort' = {# fun unsafe Abort as abort_ {fromComm `Comm', id `CInt'} -> `()' checkError*- #}


type MPIDatatype = {# type MPI_Datatype #}

-- | Haskell datatype used to represent @MPI_Datatype@. 
-- Please refer to Chapter 4 of MPI Report v. 2.2 for a description
-- of various datatypes.
newtype Datatype = MkDatatype { fromDatatype :: MPIDatatype }

foreign import ccall unsafe "&mpi_char" char_ :: Ptr MPIDatatype
foreign import ccall unsafe "&mpi_wchar" wchar_ :: Ptr MPIDatatype
foreign import ccall unsafe "&mpi_short" short_ :: Ptr MPIDatatype
foreign import ccall unsafe "&mpi_int" int_ :: Ptr MPIDatatype
foreign import ccall unsafe "&mpi_long" long_ :: Ptr MPIDatatype
foreign import ccall unsafe "&mpi_long_long" longLong_ :: Ptr MPIDatatype
foreign import ccall unsafe "&mpi_unsigned_char" unsignedChar_ :: Ptr MPIDatatype
foreign import ccall unsafe "&mpi_unsigned_short" unsignedShort_ :: Ptr MPIDatatype
foreign import ccall unsafe "&mpi_unsigned" unsigned_ :: Ptr MPIDatatype
foreign import ccall unsafe "&mpi_unsigned_long" unsignedLong_ :: Ptr MPIDatatype
foreign import ccall unsafe "&mpi_unsigned_long_long" unsignedLongLong_ :: Ptr MPIDatatype
foreign import ccall unsafe "&mpi_float" float_ :: Ptr MPIDatatype
foreign import ccall unsafe "&mpi_double" double_ :: Ptr MPIDatatype
foreign import ccall unsafe "&mpi_long_double" longDouble_ :: Ptr MPIDatatype
foreign import ccall unsafe "&mpi_byte" byte_ :: Ptr MPIDatatype
foreign import ccall unsafe "&mpi_packed" packed_ :: Ptr MPIDatatype

char, wchar, short, int, long, longLong, unsignedChar, unsignedShort :: Datatype
unsigned, unsignedLong, unsignedLongLong, float, double, longDouble :: Datatype
byte, packed :: Datatype

char = MkDatatype <$> unsafePerformIO $ peek char_
wchar = MkDatatype <$> unsafePerformIO $ peek wchar_
short = MkDatatype <$> unsafePerformIO $ peek short_
int = MkDatatype <$> unsafePerformIO $ peek int_
long = MkDatatype <$> unsafePerformIO $ peek long_
longLong = MkDatatype <$> unsafePerformIO $ peek longLong_
unsignedChar = MkDatatype <$> unsafePerformIO $ peek unsignedChar_
unsignedShort = MkDatatype <$> unsafePerformIO $ peek unsignedShort_
unsigned = MkDatatype <$> unsafePerformIO $ peek unsigned_
unsignedLong = MkDatatype <$> unsafePerformIO $ peek unsignedLong_
unsignedLongLong = MkDatatype <$> unsafePerformIO $ peek unsignedLongLong_
float = MkDatatype <$> unsafePerformIO $ peek float_
double = MkDatatype <$> unsafePerformIO $ peek double_
longDouble = MkDatatype <$> unsafePerformIO $ peek longDouble_
byte = MkDatatype <$> unsafePerformIO $ peek byte_
packed = MkDatatype <$> unsafePerformIO $ peek packed_

type MPIErrhandler = {# type MPI_Errhandler #}

-- | Haskell datatype that represents values usable as @MPI_Errhandler@
newtype Errhandler = MkErrhandler { fromErrhandler :: MPIErrhandler } deriving Storable
peekErrhandler ptr = MkErrhandler <$> peek ptr

foreign import ccall "&mpi_errors_are_fatal" errorsAreFatal_ :: Ptr MPIErrhandler
foreign import ccall "&mpi_errors_return" errorsReturn_ :: Ptr MPIErrhandler

-- | Predefined @Errhandler@ that will terminate the process on any
--   MPI error
errorsAreFatal :: Errhandler
errorsAreFatal = MkErrhandler <$> unsafePerformIO $ peek errorsAreFatal_

-- | Predefined @Errhandler@ that will convert errors into Haskell
-- exceptions. Mimics the semantics of @MPI_Errors_return@
errorsReturn :: Errhandler
errorsReturn = MkErrhandler <$> unsafePerformIO $ peek errorsReturn_

-- | Same as 'errorsReturn', but with a more meaningful name.
errorsThrowExceptions :: Errhandler
errorsThrowExceptions = errorsReturn

{# enum ErrorClass {underscoreToCase} deriving (Eq,Ord,Show,Typeable) #}

-- XXX Should this be a ForeinPtr?
-- there is a MPI_Group_free function, which we should probably
-- call when the group is no longer referenced.

-- | Actual Haskell type used depends on the MPI implementation.
type MPIGroup = {# type MPI_Group #}

-- | Haskell datatype representing MPI process groups.
newtype Group = MkGroup { fromGroup :: MPIGroup } deriving Storable
peekGroup ptr = MkGroup <$> peek ptr

foreign import ccall "&mpi_group_empty" groupEmpty_ :: Ptr MPIGroup
-- | A predefined group without any members. Corresponds to @MPI_GROUP_EMPTY@.
groupEmpty :: Group
groupEmpty = MkGroup <$> unsafePerformIO $ peek groupEmpty_


-- | Actual Haskell type used depends on the MPI implementation.
type MPIOperation = {# type MPI_Op #}

{- | Abstract type representing handle for MPI reduction operation
(that can be used with 'reduce', 'allreduce', and 'reduceScatter').
-}
newtype Operation = MkOperation { fromOperation :: MPIOperation } deriving Storable
peekOperation ptr = MkOperation <$> peek ptr
withOperation op f = alloca $ \ptr -> do poke ptr (fromOperation op)
                                         f (castPtr ptr)

foreign import ccall unsafe "&mpi_max" maxOp_ :: Ptr MPIOperation
foreign import ccall unsafe "&mpi_min" minOp_ :: Ptr MPIOperation
foreign import ccall unsafe "&mpi_sum" sumOp_ :: Ptr MPIOperation
foreign import ccall unsafe "&mpi_prod" prodOp_ :: Ptr MPIOperation
foreign import ccall unsafe "&mpi_land" landOp_ :: Ptr MPIOperation
foreign import ccall unsafe "&mpi_band" bandOp_ :: Ptr MPIOperation
foreign import ccall unsafe "&mpi_lor" lorOp_ :: Ptr MPIOperation
foreign import ccall unsafe "&mpi_bor" borOp_ :: Ptr MPIOperation
foreign import ccall unsafe "&mpi_lxor" lxorOp_ :: Ptr MPIOperation
foreign import ccall unsafe "&mpi_bxor" bxorOp_ :: Ptr MPIOperation
-- foreign import ccall "mpi_maxloc" maxlocOp :: MPIOperation
-- foreign import ccall "mpi_minloc" minlocOp :: MPIOperation
-- foreign import ccall "mpi_replace" replaceOp :: MPIOperation
-- TODO: support for those requires better support for pair datatypes

-- | Predefined reduction operation: maximum
maxOp :: Operation
maxOp = MkOperation <$> unsafePerformIO $ peek maxOp_

-- | Predefined reduction operation: minimum
minOp :: Operation
minOp = MkOperation <$> unsafePerformIO $ peek minOp_

-- | Predefined reduction operation: (+)
sumOp :: Operation
sumOp = MkOperation <$> unsafePerformIO $ peek sumOp_

-- | Predefined reduction operation: (*)
prodOp :: Operation
prodOp = MkOperation <$> unsafePerformIO $ peek prodOp_

-- | Predefined reduction operation: logical \"and\"
landOp :: Operation
landOp = MkOperation <$> unsafePerformIO $ peek landOp_

-- | Predefined reduction operation: bit-wise \"and\"
bandOp :: Operation
bandOp = MkOperation <$> unsafePerformIO $ peek bandOp_

-- | Predefined reduction operation: logical \"or\"
lorOp :: Operation
lorOp = MkOperation <$> unsafePerformIO $ peek lorOp_

-- | Predefined reduction operation: bit-wise \"or\"
borOp :: Operation
borOp = MkOperation <$> unsafePerformIO $ peek borOp_

-- | Predefined reduction operation: logical \"xor\"
lxorOp :: Operation
lxorOp = MkOperation <$> unsafePerformIO $ peek lxorOp_

-- | Predefined reduction operation: bit-wise \"xor\"
bxorOp :: Operation
bxorOp = MkOperation <$> unsafePerformIO $ peek bxorOp_

-- | Actual Haskell type used depends on the MPI implementation.
type MPIInfo = {# type MPI_Info #}

{- | Abstract type representing handle for MPI Info object
-}
newtype Info = MkInfo { fromInfo :: MPIInfo } deriving Storable
peekInfo ptr = MkInfo <$> peek ptr
withInfo op f = alloca $ \ptr -> do poke ptr (fromInfo op)
                                    f (castPtr ptr)

foreign import ccall "&mpi_info_null" infoNull_ :: Ptr MPIInfo

-- | Predefined info object that has no info
infoNull :: Info
infoNull = unsafePerformIO $ peekInfo infoNull_

{- | Haskell datatype that represents values which
 could be used as MPI rank designations. Low-level MPI calls require
 that you use 32-bit non-negative integer values as ranks, so any
 non-numeric Haskell Ranks should provide a sensible instances of
 'Enum'.

Attempt to supply a value that does not fit into 32 bits will cause a
run-time 'error'.
-}
newtype Rank = MkRank { rankId :: Int -- ^ Extract numeric value of the 'Rank'
                      }
   deriving (Eq, Ord, Enum, Integral, Real)

instance Num Rank where
  (MkRank x) + (MkRank y) = MkRank (x+y)
  (MkRank x) * (MkRank y) = MkRank (x*y)
  abs (MkRank x) = MkRank (abs x)
  signum (MkRank x) = MkRank (signum x)
  fromInteger x
    | x > ( fromIntegral (maxBound :: CInt)) = error "Rank value does not fit into 32 bits"
    | x < 0             = error "Negative Rank value"
    | otherwise         = MkRank (fromIntegral x)

foreign import ccall "&mpi_any_source" anySource_ :: Ptr Int
foreign import ccall "&mpi_root" theRoot_ :: Ptr Int
foreign import ccall "&mpi_proc_null" procNull_ :: Ptr Int
foreign import ccall "&mpi_request_null" requestNull_ :: Ptr MPIRequest
foreign import ccall "&mpi_comm_null" commNull_ :: Ptr MPIComm

-- | Predefined rank number that allows reception of point-to-point messages
-- regardless of their source. Corresponds to @MPI_ANY_SOURCE@
anySource :: Rank
anySource = toRank $ unsafePerformIO $ peek anySource_

-- | Predefined rank number that specifies root process during
-- operations involving intercommunicators. Corresponds to @MPI_ROOT@
theRoot :: Rank
theRoot = toRank $ unsafePerformIO $ peek theRoot_

-- | Predefined rank number that specifies non-root processes during
-- operations involving intercommunicators. Corresponds to @MPI_PROC_NULL@
procNull :: Rank
procNull  = toRank $ unsafePerformIO $ peek procNull_

-- | Predefined request handle value that specifies non-existing or finished request.
-- Corresponds to @MPI_REQUEST_NULL@
requestNull :: Request
requestNull  = unsafePerformIO $ peekRequest requestNull_

-- | Predefined communicator handle value that specifies non-existing or destroyed (inter-)communicator.
-- Corresponds to @MPI_COMM_NULL@
commNull :: Comm
commNull  = unsafePerformIO $ peekComm commNull_

instance Show Rank where
   show = show . rankId

-- | Map arbitrary 'Enum' value to 'Rank'
toRank :: Enum a => a -> Rank
toRank x = MkRank { rankId = fromEnum x }

-- | Map 'Rank' to arbitrary 'Enum'
fromRank :: Enum a => Rank -> a
fromRank = toEnum . rankId

type MPIRequest = {# type MPI_Request #}
{-| Haskell representation of the @MPI_Request@ type. Returned by
non-blocking communication operations, could be further processed with
'probe', 'test', 'cancel' or 'wait'. -}
newtype Request = MkRequest MPIRequest deriving (Storable,Eq)
peekRequest ptr = MkRequest <$> peek ptr

{-
This module provides Haskell representation of the @MPI_Status@ type
(request status).

Field `status_error' should be used with care:
\"The error field in status is not needed for calls that return only
one status, such as @MPI_WAIT@, since that would only duplicate the
information returned by the function itself. The current design avoids
the additional overhead of setting it, in such cases. The field is
needed for calls that return multiple statuses, since each request may
have had a different failure.\"
(this is a quote from <http://mpi-forum.org/docs/mpi22-report/node47.htm#Node47>)

This means that, for example, during the call to @MPI_Wait@
implementation is free to leave this field filled with whatever
garbage got there during memory allocation. Haskell FFI is not
blanking out freshly allocated memory, so beware!
-}

-- | Haskell structure that holds fields of @MPI_Status@.
--
-- Please note that MPI report lists only three fields as mandatory:
-- @status_source@, @status_tag@ and @status_error@. However, all
-- MPI implementations that were used to test those bindings supported
-- extended set of fields represented here.
data Status =
   Status
   { status_source :: Rank -- ^ rank of the source process
   , status_tag :: Tag -- ^ tag assigned at source
   , status_error :: CInt -- ^ error code, if any
   , status_count :: CInt -- ^ number of received elements, if applicable
   , status_cancelled :: Bool -- ^ whether the request was cancelled
   }
   deriving (Eq, Ord, Show)

instance Storable Status where
  sizeOf _ = {#sizeof MPI_Status #}
  alignment _ = 4
  peek p = Status
    <$> liftM (MkRank . cIntConv) ({#get MPI_Status->MPI_SOURCE #} p)
    <*> liftM (MkTag . cIntConv) ({#get MPI_Status->MPI_TAG #} p)
    <*> liftM cIntConv ({#get MPI_Status->MPI_ERROR #} p)
#ifdef MPICH2
    -- MPICH2 and OpenMPI use different names for the status struct
    -- fields-
    <*> liftM cIntConv ({#get MPI_Status->count #} p)
    <*> liftM cToEnum ({#get MPI_Status->cancelled #} p)
#else
    <*> liftM cIntConv ({#get MPI_Status->_count #} p)
    <*> liftM cToEnum ({#get MPI_Status->_cancelled #} p)
#endif
  poke p x = do
    {#set MPI_Status.MPI_SOURCE #} p (fromRank $ status_source x)
    {#set MPI_Status.MPI_TAG #} p (fromTag $ status_tag x)
    {#set MPI_Status.MPI_ERROR #} p (cIntConv $ status_error x)
#ifdef MPICH2
    -- MPICH2 and OpenMPI use different names for the status struct
    -- fields AND different order of fields
    {#set MPI_Status.count #} p (cIntConv $ status_count x)
    {#set MPI_Status.cancelled #} p (cFromEnum $ status_cancelled x)
#else
    {#set MPI_Status._count #} p (cIntConv $ status_count x)
    {#set MPI_Status._cancelled #} p (cFromEnum $ status_cancelled x)
#endif

-- NOTE: Int here is picked arbitrary
allocaCast f =
  alloca $ \(ptr :: Ptr Int) -> f (castPtr ptr :: Ptr ())
peekCast = peek . castPtr


{-| Haskell datatype that represents values which could be used as
tags for point-to-point transfers.
-}
newtype Tag = MkTag { tagVal :: Int -- ^ Extract numeric value of the Tag
                    }
   deriving (Eq, Ord, Enum, Integral, Real)

instance Num Tag where
  (MkTag x) + (MkTag y) = MkTag (x+y)
  (MkTag x) * (MkTag y) = MkTag (x*y)
  abs (MkTag x) = MkTag (abs x)
  signum (MkTag x) = MkTag (signum x)
  fromInteger x
    | fromIntegral x > tagUpperBound = error "Tag value is over the MPI_TAG_UB"
    | x < 0             = error "Negative Tag value"
    | otherwise         = MkTag (fromIntegral x)

instance Show Tag where
  show = show . tagVal

-- | Map arbitrary 'Enum' value to 'Tag'
toTag :: Enum a => a -> Tag
toTag x = MkTag { tagVal = fromEnum x }

-- | Map 'Tag' to arbitrary 'Enum'
fromTag :: Enum a => Tag -> a
fromTag = toEnum . tagVal

foreign import ccall unsafe "&mpi_any_tag" anyTag_ :: Ptr Int

-- | Predefined tag value that allows reception of the messages with
--   arbitrary tag values. Corresponds to @MPI_ANY_TAG@.
anyTag :: Tag
anyTag = toTag $ unsafePerformIO $ peek anyTag_

-- | A tag with unit value. Intended to be used as a convenient default.
unitTag :: Tag
unitTag = toTag ()

{- | Constants used to describe the required level of multithreading
   support in call to 'initThread'. They also describe provided level
   of multithreading support as returned by 'queryThread' and
   'initThread'.

[@Single@]  Only one thread will execute.

[@Funneled@] The process may be multi-threaded, but the application must
ensure that only the main thread makes MPI calls (see 'isThreadMain').

[@Serialized@] The process may be multi-threaded, and multiple threads may
make MPI calls, but only one at a time: MPI calls are not made concurrently from
two distinct threads

[@Multiple@] Multiple threads may call MPI, with no restrictions.

XXX Make sure we have the correct ordering as defined by MPI. Also we should
describe the ordering here (other parts of the docs need it to be explained - see initThread).

-}
{# enum ThreadSupport {underscoreToCase} deriving (Eq,Ord,Show) #}

-- | Value thrown as exception when MPI runtime is instructed to pass
--   errors to user code (via 'commSetErrhandler' and 'errorsReturn').
-- Since raw MPI errors codes are not standartized and not portable,
-- they are not exposed.
data MPIError
   = MPIError
     { mpiErrorClass :: ErrorClass -- ^ Broad class of errors this one belongs to
     , mpiErrorString :: String -- ^ Human-readable error description
     }
   deriving (Eq, Show, Typeable)

instance Exception MPIError

checkError :: CInt -> IO ()
checkError code = do
   -- We ignore the error code from the call to Internal.errorClass
   -- because we call errorClass from checkError. We'd end up
   -- with an infinite loop if we called checkError here.
   (_, errClassRaw) <- errorClass code
   let errClass = cToEnum errClassRaw
   unless (errClass == Success) $ do
      errStr <- errorString code
      throwIO $ MPIError errClass errStr

-- | Convert MPI error code human-readable error description. Corresponds to @MPI_Error_string@.
errorString :: CInt -> IO String
errorString code =
  allocaBytes (fromIntegral maxErrorString) $ \ptr ->
    alloca $ \lenPtr -> do
       -- We ignore the error code from the call to Internal.errorString
       -- because we call errorString from checkError. We'd end up
       -- with an infinite loop if we called checkError here.
       _ <- errorString' code ptr lenPtr
       len <- peek lenPtr
       peekCStringLen (ptr, cIntConv len)
  where
    errorString' = {# call unsafe Error_string as errorString_ #}
