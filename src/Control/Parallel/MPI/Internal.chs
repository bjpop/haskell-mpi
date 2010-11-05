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
All Haskell functions correspond to MPI functions with the similar
name (i.e. @commRank@ is the binding for @MPI_Comm_rank@)

Actual types of all functions defined here depend on the MPI
implementation.

Since "Control.Parallel.MPI.Storable" and
"Control.Parallel.MPI.Serializable" contains many functions of the
same name as defined here, users would want to import this module
qualified.
-}
-----------------------------------------------------------------------------
module Control.Parallel.MPI.Internal
   ( maxProcessorName,
     maxErrorString,
     init, initThread, queryThread, isThreadMain, initialized, finalized,
     finalize, getProcessorName, getVersion, Version(..),
     send, bsend, ssend, rsend, recv,
     commRank, probe, commSize, commTestInter, commRemoteSize,
     commCompare, commGetAttr,
     isend, ibsend, issend, isendPtr, ibsendPtr, issendPtr, irecv, irecvPtr, bcast, barrier, wait, waitall, test,
     cancel, scatter, gather,
     scatterv, gatherv,
     allgather, allgatherv,
     alltoall, alltoallv,
     reduce, allreduce, reduceScatter,
     opCreate, opFree,
     wtime, wtick,
     commGroup, groupRank, groupSize, groupUnion, groupIntersection, groupDifference,
     groupCompare, groupExcl, groupIncl, groupTranslateRanks,
     typeSize,
     errorClass, errorString, commSetErrhandler, commGetErrhandler,
     abort,
     Comm(), commWorld, commSelf,
     ComparisonResult (..),
     Datatype(), char, wchar, short, int, long, longLong, unsignedChar,
     unsignedShort, unsigned, unsignedLong, unsignedLongLong, float, double,
     longDouble, byte, packed,
     Errhandler, errorsAreFatal, errorsReturn,
     ErrorClass (..), MPIError(..),
     Group(), groupEmpty,
     Operation(), maxOp, minOp, sumOp, prodOp, landOp, bandOp, lorOp,
     borOp, lxorOp, bxorOp,
     Rank(), rankId, toRank, fromRank, anySource, theRoot, procNull,
     Request(),
     Status (..),
     Tag(), toTag, fromTag, tagVal, anyTag, tagUpperBound,
     ThreadSupport (..)
   ) where

import Prelude hiding (init)
import C2HS
import Data.Typeable
import Data.Maybe (fromMaybe)
import Control.Monad (liftM, unless)
import Control.Applicative ((<$>), (<*>))
import Control.Exception

{# context prefix = "MPI" #}

type BufferPtr = Ptr ()
type Count = CInt

{-
This module provides Haskell enum that comprises of MPI constants
@MPI_IDENT@, @MPI_CONGRUENT@, @MPI_SIMILAR@ and @MPI_UNEQUAL@.

Those are used to compare communicators (f.e. 'commCompare') and
process groups (f.e. 'groupCompare').
-}
{# enum ComparisonResult {underscoreToCase} deriving (Eq,Ord,Show) #}

-- | Which Haskell type will be used as @Comm@ depends on the MPI
-- implementation that was selected during compilation. It could be
-- @CInt@, @Ptr ()@, @Ptr CInt@ or something else.
type MPIComm = {# type MPI_Comm #}
newtype Comm = MkComm { fromComm :: MPIComm }
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
maxProcessorName :: CInt
maxProcessorName = unsafePerformIO $ peek max_processor_name_
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

{# fun unsafe Query_thread as ^ {alloca- `Bool' peekBool* } -> `()' checkError*- #}

{# fun unsafe Is_thread_main as ^
                 {alloca- `Bool' peekBool* } -> `()' checkError*- #}

-- | Terminate the MPI execution environment.
-- Once 'finalize' is called no other MPI functions may be called except
-- 'getVersion', 'initialized' and 'finalized'. Each process must complete
-- any pending communication that it initiated before calling 'finalize'.
-- If 'finalize' returns then regular (non-MPI) computations may continue,
-- but no further MPI computation is possible. Note: the error code returned
-- by 'finalize' is not checked. This function corresponds to @MPI_Finalize@.
{# fun unsafe Finalize as ^ {} -> `()' discard*- #}
discard _ = return ()
-- XXX can't call checkError on finalize, because
-- checkError calls Internal.errorClass and Internal.errorString.
-- These cannot be called after finalize (at least on OpenMPI).

getProcessorName :: IO String
getProcessorName = do
  allocaBytes (fromIntegral maxProcessorName) $ \ptr -> do
    len <- getProcessorName' ptr
    peekCStringLen (ptr, cIntConv len)
  where
    getProcessorName' = {# fun unsafe Get_processor_name as getProcessorName_
                           {id `Ptr CChar', alloca- `CInt' peekIntConv*} -> `()' checkError*- #}

data Version =
   Version { version :: Int, subversion :: Int }
   deriving (Eq, Ord)

instance Show Version where
   show v = show (version v) ++ "." ++ show (subversion v)

getVersion :: IO Version
getVersion = do
   (version, subversion) <- getVersion'
   return $ Version version subversion
  where
    getVersion' = {# fun unsafe Get_version as getVersion_
                     {alloca- `Int' peekIntConv*, alloca- `Int' peekIntConv*} -> `()' checkError*- #}

-- | Return the number of processes involved in a communicator. For 'commWorld'
-- it returns the total number of processes available. If the communicator is
-- and intra-communicator it returns the number of processes in the local group.
-- This function corresponds to @MPI_Comm_size@.
{# fun unsafe Comm_size as ^
              {fromComm `Comm', alloca- `Int' peekIntConv* } -> `()' checkError*- #}

{# fun unsafe Comm_remote_size as ^
                    {fromComm `Comm', alloca- `Int' peekIntConv* } -> `()' checkError*- #}

{# fun unsafe Comm_test_inter as ^
                   {fromComm `Comm', alloca- `Bool' peekBool* } -> `()' checkError*- #}

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

foreign import ccall unsafe "&mpi_tag_ub" tagUB_ :: Ptr Int

-- | Predefined tag value that allows reception of the messages with
--   arbitrary tag values. Corresponds to @MPI_ANY_TAG@.
tagUpperBound :: Int
tagUpperBound =
  let key = unsafePerformIO (peek tagUB_) 
      in fromMaybe 0 $ unsafePerformIO (commGetAttr commWorld key)

-- | Return the rank of the calling process for the given communicator.
-- This function corresponds to @MPI_Comm_rank@.
{# fun unsafe Comm_rank as ^
              {fromComm `Comm', alloca- `Rank' peekIntConv* } -> `()' checkError*- #}

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

{# fun unsafe Send as ^
          { id `BufferPtr', id `Count', fromDatatype `Datatype', fromRank `Rank', fromTag `Tag', fromComm `Comm' } -> `()' checkError*- #}
{# fun unsafe Bsend as ^
          { id `BufferPtr', id `Count', fromDatatype `Datatype', fromRank `Rank', fromTag `Tag', fromComm `Comm' } -> `()' checkError*- #}
{# fun unsafe Ssend as ^
          { id `BufferPtr', id `Count', fromDatatype `Datatype', fromRank `Rank', fromTag `Tag', fromComm `Comm' } -> `()' checkError*- #}
{# fun unsafe Rsend as ^
          { id `BufferPtr', id `Count', fromDatatype `Datatype', fromRank `Rank', fromTag `Tag', fromComm `Comm' } -> `()' checkError*- #}
{# fun unsafe Recv as ^
          { id `BufferPtr', id `Count', fromDatatype `Datatype', fromRank `Rank', fromTag `Tag', fromComm `Comm', allocaCast- `Status' peekCast* } -> `()' checkError*- #}
{# fun unsafe Isend as ^
           { id `BufferPtr', id `Count', fromDatatype `Datatype', fromRank `Rank', fromTag `Tag', fromComm `Comm', alloca- `Request' peekRequest*} -> `()' checkError*- #}
{# fun unsafe Ibsend as ^
           { id `BufferPtr', id `Count', fromDatatype `Datatype', fromRank `Rank', fromTag `Tag', fromComm `Comm', alloca- `Request' peekRequest*} -> `()' checkError*- #}
{# fun unsafe Issend as ^
           { id `BufferPtr', id `Count', fromDatatype `Datatype', fromRank `Rank', fromTag `Tag', fromComm `Comm', alloca- `Request' peekRequest*} -> `()' checkError*- #}
{# fun unsafe Isend as isendPtr
           { id `BufferPtr', id `Count', fromDatatype `Datatype', fromRank `Rank', fromTag `Tag', fromComm `Comm', castPtr `Ptr Request'} -> `()' checkError*- #}
{# fun unsafe Ibsend as ibsendPtr
           { id `BufferPtr', id `Count', fromDatatype `Datatype', fromRank `Rank', fromTag `Tag', fromComm `Comm', castPtr `Ptr Request'} -> `()' checkError*- #}
{# fun unsafe Issend as issendPtr
           { id `BufferPtr', id `Count', fromDatatype `Datatype', fromRank `Rank', fromTag `Tag', fromComm `Comm', castPtr `Ptr Request'} -> `()' checkError*- #}
{# fun Irecv as ^
           { id `BufferPtr', id `Count', fromDatatype `Datatype', fromRank `Rank', fromTag `Tag', fromComm `Comm', alloca- `Request' peekRequest*} -> `()' checkError*- #}
{# fun Irecv as irecvPtr
           { id `BufferPtr', id `Count', fromDatatype `Datatype', fromRank `Rank', fromTag `Tag', fromComm `Comm', castPtr `Ptr Request'} -> `()' checkError*- #}
{# fun unsafe Bcast as ^
           { id `BufferPtr', id `Count', fromDatatype `Datatype', fromRank `Rank', fromComm `Comm'} -> `()' checkError*- #}

-- | Blocks until all processes on the communicator call this function.
-- This function corresponds to @MPI_Barrier@.
{# fun unsafe Barrier as ^ {fromComm `Comm'} -> `()' checkError*- #}

-- | Blocking test for the completion of a send of receive.
-- See 'test' for a non-blocking variant.
-- This function corresponds to @MPI_Wait@.
{# fun unsafe Wait as ^
          {withRequest* `Request', allocaCast- `Status' peekCast*} -> `()' checkError*-  #}

-- TODO: Make this Storable Array instead of Ptr ?
{# fun unsafe Waitall as ^
            { id `Count', castPtr `Ptr Request', castPtr `Ptr Status'} -> `()' checkError*- #}

-- | Non-blocking test for the completion of a send or receive.
-- Returns @Nothing@ if the request is not complete, otherwise
-- it returns @Just status@. See 'wait' for a blocking variant.
-- This function corresponds to @MPI_Test@.
test :: Request -> IO (Maybe Status)
test request = do
  (flag, status) <- test' request
  if flag
     then return $ Just status
     else return Nothing
  where
    test' = {# fun unsafe Test as test_
              {withRequest* `Request', alloca- `Bool' peekBool*, allocaCast- `Status' peekCast*} -> `()' checkError*- #}

-- | Cancel a pending communication request.
-- This function corresponds to @MPI_Cancel@.
{# fun unsafe Cancel as ^
            {withRequest* `Request'} -> `()' checkError*- #}
withRequest req f = do alloca $ \ptr -> do poke ptr req
                                           f (castPtr ptr)
{# fun unsafe Scatter as ^
             { id `BufferPtr', id `Count', fromDatatype `Datatype',
               id `BufferPtr', id `Count', fromDatatype `Datatype',
               fromRank `Rank', fromComm `Comm'} -> `()' checkError*- #}

{# fun unsafe Gather as ^
             { id `BufferPtr', id `Count', fromDatatype `Datatype',
               id `BufferPtr', id `Count', fromDatatype `Datatype',
               fromRank `Rank', fromComm `Comm'} -> `()' checkError*- #}

-- We pass counts/displs as Ptr CInt so that caller could supply nullPtr here
-- which would be impossible if we marshal arrays ourselves here.
{# fun unsafe Scatterv as ^
             { id `BufferPtr', id `Ptr CInt', id `Ptr CInt', fromDatatype `Datatype',
               id `BufferPtr', id `Count', fromDatatype `Datatype',
               fromRank `Rank', fromComm `Comm'} -> `()' checkError*- #}
{# fun unsafe Gatherv as ^
             { id `BufferPtr', id `Count', fromDatatype `Datatype',
               id `BufferPtr', id `Ptr CInt', id `Ptr CInt', fromDatatype `Datatype',
               fromRank `Rank', fromComm `Comm'} -> `()' checkError*- #}
{# fun unsafe Allgather as ^
             { id `BufferPtr', id `Count', fromDatatype `Datatype',
               id `BufferPtr', id `Count', fromDatatype `Datatype',
               fromComm `Comm'} -> `()' checkError*- #}
{# fun unsafe Allgatherv as ^
             { id `BufferPtr', id `Count', fromDatatype `Datatype',
               id `BufferPtr', id `Ptr CInt', id `Ptr CInt', fromDatatype `Datatype',
               fromComm `Comm'} -> `()' checkError*- #}
{# fun unsafe Alltoall as ^
             { id `BufferPtr', id `Count', fromDatatype `Datatype',
               id `BufferPtr', id `Count', fromDatatype `Datatype',
               fromComm `Comm'} -> `()' checkError*- #}
{# fun unsafe Alltoallv as ^
             { id `BufferPtr', id `Ptr CInt', id `Ptr CInt', fromDatatype `Datatype',
               id `BufferPtr', id `Ptr CInt', id `Ptr CInt', fromDatatype `Datatype',
               fromComm `Comm'} -> `()' checkError*- #}
-- Reduce, allreduce and reduceScatter could call back to Haskell
-- via user-defined ops, so they should be imported in "safe" mode
{# fun Reduce as ^
             { id `BufferPtr', id `BufferPtr', id `Count', fromDatatype `Datatype',
               fromOperation `Operation', fromRank `Rank', fromComm `Comm'} -> `()' checkError*- #}
{# fun Allreduce as ^
             { id `BufferPtr', id `BufferPtr', id `Count', fromDatatype `Datatype',
               fromOperation `Operation', fromComm `Comm'} -> `()' checkError*- #}
{# fun Reduce_scatter as ^
             { id `BufferPtr', id `BufferPtr', id `Ptr CInt', fromDatatype `Datatype',
               fromOperation `Operation', fromComm `Comm'} -> `()' checkError*- #}
{# fun unsafe Op_create as ^
              {castFunPtr `FunPtr (Ptr t -> Ptr t -> Ptr CInt -> Ptr Datatype -> IO ())', cFromEnum `Bool', alloca- `Operation' peekOperation*} -> `()' checkError*- #}
opFree = {# call unsafe Op_free as opFree_ #}
{# fun unsafe Wtime as ^ {} -> `Double' realToFrac #}
{# fun unsafe Wtick as ^ {} -> `Double' realToFrac #}

-- | Return the process group from a communicator.
{# fun unsafe Comm_group as ^
               {fromComm `Comm', alloca- `Group' peekGroup*} -> `()' checkError*- #}

groupRank = unsafePerformIO <$> groupRank'
  where groupRank' = {# fun unsafe Group_rank as groupRank_
                        {fromGroup `Group', alloca- `Rank' peekIntConv*} -> `()' checkError*- #}

groupSize = unsafePerformIO <$> groupSize'
  where groupSize' = {# fun unsafe Group_size as groupSize_
                        {fromGroup `Group', alloca- `Int' peekIntConv*} -> `()' checkError*- #}

groupUnion g1 g2 = unsafePerformIO $ groupUnion' g1 g2
  where groupUnion' = {# fun unsafe Group_union as groupUnion_
                         {fromGroup `Group', fromGroup `Group', alloca- `Group' peekGroup*} -> `()' checkError*- #}

groupIntersection g1 g2 = unsafePerformIO $ groupIntersection' g1 g2
  where groupIntersection' = {# fun unsafe Group_intersection as groupIntersection_
                                {fromGroup `Group', fromGroup `Group', alloca- `Group' peekGroup*} -> `()' checkError*- #}

groupDifference g1 g2 = unsafePerformIO $ groupDifference' g1 g2
  where groupDifference' = {# fun unsafe Group_difference as groupDifference_
                              {fromGroup `Group', fromGroup `Group', alloca- `Group' peekGroup*} -> `()' checkError*- #}

groupCompare g1 g2 = unsafePerformIO $ groupCompare' g1 g2
  where
    groupCompare' = {# fun unsafe Group_compare as groupCompare_
                       {fromGroup `Group', fromGroup `Group', alloca- `ComparisonResult' peekEnum*} -> `()' checkError*- #}

-- Technically it might make better sense to make the second argument a Set rather than a list
-- but the order is significant in the groupIncl function (the other function, not this one).
-- For the sake of keeping their types in sync, a list is used instead.
{# fun unsafe Group_excl as ^
               {fromGroup `Group', withRanksAsInts* `[Rank]'&, alloca- `Group' peekGroup*} -> `()' checkError*- #}
{# fun unsafe Group_incl as ^
               {fromGroup `Group', withRanksAsInts* `[Rank]'&, alloca- `Group' peekGroup*} -> `()' checkError*- #}

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

-- | Return the number of bytes used to store an MPI 'Datatype'.
typeSize = unsafePerformIO . typeSize'
  where
    typeSize' =
      {# fun unsafe Type_size as typeSize_
         {fromDatatype `Datatype', alloca- `Int' peekIntConv*} -> `()' checkError*- #}

{# fun unsafe Error_class as ^
                { id `CInt', alloca- `CInt' peek*} -> `CInt' id #}
errorString = {# call unsafe Error_string as errorString_ #}

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
newtype Errhandler = MkErrhandler { fromErrhandler :: MPIErrhandler } deriving Storable
peekErrhandler ptr = MkErrhandler <$> peek ptr

foreign import ccall "&mpi_errors_are_fatal" errorsAreFatal_ :: Ptr MPIErrhandler
foreign import ccall "&mpi_errors_return" errorsReturn_ :: Ptr MPIErrhandler
errorsAreFatal, errorsReturn :: Errhandler
errorsAreFatal = MkErrhandler <$> unsafePerformIO $ peek errorsAreFatal_
errorsReturn = MkErrhandler <$> unsafePerformIO $ peek errorsReturn_

{# enum ErrorClass {underscoreToCase} deriving (Eq,Ord,Show,Typeable) #}

-- XXX Should this be a ForeinPtr?
-- there is a MPI_Group_free function, which we should probably
-- call when the group is no longer referenced.

-- | Actual Haskell type used depends on the MPI implementation.
type MPIGroup = {# type MPI_Group #}
newtype Group = MkGroup { fromGroup :: MPIGroup } deriving Storable
peekGroup ptr = MkGroup <$> peek ptr

foreign import ccall "&mpi_group_empty" groupEmpty_ :: Ptr MPIGroup
-- | Predefined handle for group without any members. Corresponds to @MPI_GROUP_EMPTY@
groupEmpty :: Group
groupEmpty = MkGroup <$> unsafePerformIO $ peek groupEmpty_


{-
This module provides Haskell type that represents values of @MPI_Op@
type (reduction operations), and predefined reduction operations
defined in the MPI Report.
-}

-- | Actual Haskell type used depends on the MPI implementation.
type MPIOperation = {# type MPI_Op #}
newtype Operation = MkOperation { fromOperation :: MPIOperation } deriving Storable
peekOperation ptr = MkOperation <$> peek ptr

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

maxOp, minOp, sumOp, prodOp, landOp, bandOp, lorOp, borOp, lxorOp, bxorOp :: Operation
maxOp = MkOperation <$> unsafePerformIO $ peek maxOp_
minOp = MkOperation <$> unsafePerformIO $ peek minOp_
sumOp = MkOperation <$> unsafePerformIO $ peek sumOp_
prodOp = MkOperation <$> unsafePerformIO $ peek prodOp_
landOp = MkOperation <$> unsafePerformIO $ peek landOp_
bandOp = MkOperation <$> unsafePerformIO $ peek bandOp_
lorOp = MkOperation <$> unsafePerformIO $ peek lorOp_
borOp = MkOperation <$> unsafePerformIO $ peek borOp_
lxorOp = MkOperation <$> unsafePerformIO $ peek lxorOp_
bxorOp = MkOperation <$> unsafePerformIO $ peek bxorOp_


{-
This module provides Haskell datatype that represents values which
could be used as MPI rank designations.
-}

newtype Rank = MkRank { rankId :: Int }
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

foreign import ccall "mpi_any_source" anySource_ :: Ptr Int
foreign import ccall "mpi_root" theRoot_ :: Ptr Int
foreign import ccall "mpi_proc_null" procNull_ :: Ptr Int

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

instance Show Rank where
   show = show . rankId

toRank :: Enum a => a -> Rank
toRank x = MkRank { rankId = fromEnum x }

fromRank :: Enum a => Rank -> a
fromRank = toEnum . rankId

{- This module provides Haskell representation of the @MPI_Request@ type. -}
type MPIRequest = {# type MPI_Request #}
newtype Request = MkRequest MPIRequest deriving Storable
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
   { status_source :: CInt -- ^ rank of the source process
   , status_tag :: CInt -- ^ tag assigned at source
   , status_error :: CInt -- ^ error code, if any
   , status_count :: CInt -- ^ number of received elements, if applicable
   , status_cancelled :: CInt -- ^ whether the request was cancelled
   }
   deriving (Eq, Ord, Show)

instance Storable Status where
  sizeOf _ = {#sizeof MPI_Status #}
  alignment _ = 4
  peek p = Status
    <$> liftM cIntConv ({#get MPI_Status->MPI_SOURCE #} p)
    <*> liftM cIntConv ({#get MPI_Status->MPI_TAG #} p)
    <*> liftM cIntConv ({#get MPI_Status->MPI_ERROR #} p)
#ifdef MPICH2
    -- MPICH2 and OpenMPI use different names for the status struct
    -- fields-
    <*> liftM cIntConv ({#get MPI_Status->count #} p)
    <*> liftM cIntConv ({#get MPI_Status->cancelled #} p)
#else
    <*> liftM cIntConv ({#get MPI_Status->_count #} p)
    <*> liftM cIntConv ({#get MPI_Status->_cancelled #} p)
#endif
  poke p x = do
    {#set MPI_Status.MPI_SOURCE #} p (cIntConv $ status_source x)
    {#set MPI_Status.MPI_TAG #} p (cIntConv $ status_tag x)
    {#set MPI_Status.MPI_ERROR #} p (cIntConv $ status_error x)
#ifdef MPICH2
    -- MPICH2 and OpenMPI use different names for the status struct
    -- fields AND different order of fields
    {#set MPI_Status.count #} p (cIntConv $ status_count x)
    {#set MPI_Status.cancelled #} p (cIntConv $ status_cancelled x)
#else
    {#set MPI_Status._count #} p (cIntConv $ status_count x)
    {#set MPI_Status._cancelled #} p (cIntConv $ status_cancelled x)
#endif

-- NOTE: Int here is picked arbitrary
allocaCast f =
  alloca $ \(ptr :: Ptr Int) -> f (castPtr ptr :: Ptr ())
peekCast = peek . castPtr


{-
This module provides Haskell datatype that represents values which
could be used as MPI tags.
-}

newtype Tag = MkTag { tagVal :: Int }
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

toTag :: Enum a => a -> Tag
toTag x = MkTag { tagVal = fromEnum x }

fromTag :: Enum a => Tag -> a
fromTag = toEnum . tagVal

foreign import ccall unsafe "&mpi_any_tag" anyTag_ :: Ptr Int

-- | Predefined tag value that allows reception of the messages with
--   arbitrary tag values. Corresponds to @MPI_ANY_TAG@.
anyTag :: Tag
anyTag = toTag $ unsafePerformIO $ peek anyTag_

{-
This module provides Haskell datatypes that comprises of values of
predefined MPI constants @MPI_THREAD_SINGLE@, @MPI_THREAD_FUNNELED@,
@MPI_THREAD_SERIALIZED@, @MPI_THREAD_MULTIPLE@.
-}

{# enum ThreadSupport {underscoreToCase} deriving (Eq,Ord,Show) #}

{- From MPI 2.2 report:
 "To make it possible for an application to interpret an error code, the routine
 MPI_ERROR_CLASS converts any error code into one of a small set of standard
 error codes"
-}

data MPIError
   = MPIError
     { mpiErrorClass :: ErrorClass
     , mpiErrorString :: String
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
      errStr <- errorStringWrapper code
      throwIO $ MPIError errClass errStr

errorStringWrapper :: CInt -> IO String
errorStringWrapper code =
  allocaBytes (fromIntegral maxErrorString) $ \ptr ->
    alloca $ \lenPtr -> do
       -- We ignore the error code from the call to Internal.errorString
       -- because we call errorString from checkError. We'd end up
       -- with an infinite loop if we called checkError here.
       _ <- errorString code ptr lenPtr
       len <- peek lenPtr
       peekCStringLen (ptr, cIntConv len)
