{-# LANGUAGE ForeignFunctionInterface, DeriveDataTypeable, GeneralizedNewtypeDeriving #-}
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
     finalize, getProcessorName, getVersion,
     send, bsend, ssend, rsend, recv,
     commRank, probe, commSize, commTestInter, commRemoteSize,
     commCompare,
     isend, ibsend, issend, irecv, bcast, barrier, wait, waitall, test,
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
     Comm, commWorld, commSelf,
     ComparisonResult (..),
     Datatype, char, wchar, short, int, long, longLong, unsignedChar,
     unsignedShort, unsigned, unsignedLong, unsignedLongLong, float, double,
     longDouble, byte, packed,
     Errhandler, errorsAreFatal, errorsReturn,
     ErrorClass (..),
     Group(MkGroup), groupEmpty,
     Operation, maxOp, minOp, sumOp, prodOp, landOp, bandOp, lorOp,
     borOp, lxorOp, bxorOp,
     Rank, rankId, toRank, fromRank, anySource, theRoot, procNull,
     Request,
     Status (..), StatusPtr,
     Tag, toTag, fromTag, tagVal, anyTag,
     ThreadSupport (..)
   ) where

import Prelude hiding (init)
import C2HS
import Data.Typeable
import Control.Monad (liftM)
import Control.Applicative ((<$>), (<*>))

{# context prefix = "MPI" #}


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
type Comm = {# type MPI_Comm #} 
foreign import ccall "&mpi_comm_world" commWorld_ :: Ptr Comm
foreign import ccall "&mpi_comm_self" commSelf_ :: Ptr Comm

-- | Predefined handle for communicator that includes all running
-- processes. Similar to @MPI_Comm_world@
commWorld :: Comm
commWorld = unsafePerformIO $ peek commWorld_

-- | Predefined handle for communicator that includes only current
-- process. Similar to @MPI_Comm_self@
commSelf :: Comm
commSelf = unsafePerformIO $ peek commSelf_

foreign import ccall "&mpi_max_processor_name" max_processor_name_ :: Ptr CInt
foreign import ccall "&mpi_max_error_string" max_error_string_ :: Ptr CInt
maxProcessorName :: CInt
maxProcessorName = unsafePerformIO $ peek max_processor_name_
maxErrorString :: CInt
maxErrorString = unsafePerformIO $ peek max_error_string_

init = {# call unsafe init_wrapper as init_wrapper_ #}
initialized = {# call unsafe Initialized as initialized_ #}
finalized = {# call unsafe Finalized as finalized_ #}
initThread = {# call unsafe init_wrapper_thread as init_wrapper_thread_ #}
queryThread = {# call unsafe Query_thread as queryThread_ #}
isThreadMain = {# call unsafe Is_thread_main as isThreadMain_ #}
finalize = {# call unsafe Finalize as finalize_ #}
getProcessorName = {# call unsafe Get_processor_name as getProcessorName_ #}
getVersion = {# call unsafe Get_version as getVersion_ #}
commSize = {# call unsafe Comm_size as commSize_ #}
commRank = {# call unsafe Comm_rank as commRank_ #}
commTestInter = {# call unsafe Comm_test_inter as commTestInter_ #}
commRemoteSize = {# call unsafe Comm_remote_size as commRemoteSize_ #}
commCompare = {# call unsafe Comm_compare as commCompare_ #}
probe = {# call Probe as probe_ #}
send = {# call unsafe Send as send_ #}
bsend = {# call unsafe Bsend as bsend_ #}
ssend = {# call unsafe Ssend as ssend_ #}
rsend = {# call unsafe Rsend as rsend_ #}
recv = {# call unsafe Recv as recv_ #}
isend = {# call unsafe Isend as isend_ #}
ibsend = {# call unsafe Ibsend as ibsend_ #}
issend = {# call unsafe Issend as issend_ #}
irecv = {# call Irecv as irecv_ #}
bcast = {# call unsafe Bcast as bcast_ #}
barrier = {# call unsafe Barrier as barrier_ #}
wait = {# call unsafe Wait as wait_ #}
waitall = {# call unsafe Waitall as waitall_ #}
test = {# call unsafe Test as test_ #}
cancel = {# call unsafe Cancel as cancel_ #}
scatter = {# call unsafe Scatter as scatter_ #}
gather = {# call unsafe Gather as gather_ #}
scatterv = {# call unsafe Scatterv as scatterv_ #}
gatherv = {# call unsafe Gatherv as gatherv_ #}
allgather = {# call unsafe Allgather as allgather_ #}
allgatherv = {# call unsafe Allgatherv as allgatherv_ #}
alltoall = {# call unsafe Alltoall as alltoall_ #}
alltoallv = {# call unsafe Alltoallv as alltoallv_ #}
-- Reduce, allreduce and reduceScatter could call back to Haskell
-- via user-defined ops, so they should be imported in "safe" mode
reduce = {# call Reduce as reduce_ #}
allreduce = {# call Allreduce as allreduce_ #}
reduceScatter = {# call Reduce_scatter as reduceScatter_ #}
opCreate = {# call unsafe Op_create as opCreate_ #}
opFree = {# call unsafe Op_free as opFree_ #}
wtime = {# call unsafe Wtime as wtime_ #}
wtick = {# call unsafe Wtick as wtick_ #}
commGroup = {# call unsafe Comm_group as commGroup_ #}
groupRank = {# call unsafe Group_rank as groupRank_ #} <$> fromGroup
groupSize = {# call unsafe Group_size as groupSize_ #} <$> fromGroup
groupUnion g1 g2 = {# call unsafe Group_union as groupUnion_ #} (fromGroup g1) (fromGroup g2)
groupIntersection g1 g2 = {# call unsafe Group_intersection as groupIntersection_ #} (fromGroup g1) (fromGroup g2)
groupDifference g1 g2 = {# call unsafe Group_difference as groupDifference_ #} (fromGroup g1) (fromGroup g2)
groupCompare g1 g2 = {# call unsafe Group_compare as groupCompare_ #} (fromGroup g1) (fromGroup g2)
groupExcl g = {# call unsafe Group_excl as groupExcl_ #} (fromGroup g)
groupIncl g = {# call unsafe Group_incl as groupIncl_ #} (fromGroup g)
groupTranslateRanks g1 s r g2 = {# call unsafe Group_translate_ranks as groupTranslateRanks_ #} (fromGroup g1) s r (fromGroup g2)
typeSize = {# call unsafe Type_size as typeSize_ #}
errorClass = {# call unsafe Error_class as errorClass_ #}
errorString = {# call unsafe Error_string as errorString_ #}
commSetErrhandler = {# call unsafe Comm_set_errhandler as commSetErrhandler_ #}
commGetErrhandler = {# call unsafe Comm_get_errhandler as commGetErrhandler_ #}
abort = {# call unsafe Abort as abort_ #}


type Datatype = {# type MPI_Datatype #}

foreign import ccall unsafe "&mpi_char" char_ :: Ptr Datatype
foreign import ccall unsafe "&mpi_wchar" wchar_ :: Ptr Datatype
foreign import ccall unsafe "&mpi_short" short_ :: Ptr Datatype
foreign import ccall unsafe "&mpi_int" int_ :: Ptr Datatype
foreign import ccall unsafe "&mpi_long" long_ :: Ptr Datatype
foreign import ccall unsafe "&mpi_long_long" longLong_ :: Ptr Datatype
foreign import ccall unsafe "&mpi_unsigned_char" unsignedChar_ :: Ptr Datatype
foreign import ccall unsafe "&mpi_unsigned_short" unsignedShort_ :: Ptr Datatype
foreign import ccall unsafe "&mpi_unsigned" unsigned_ :: Ptr Datatype
foreign import ccall unsafe "&mpi_unsigned_long" unsignedLong_ :: Ptr Datatype
foreign import ccall unsafe "&mpi_unsigned_long_long" unsignedLongLong_ :: Ptr Datatype
foreign import ccall unsafe "&mpi_float" float_ :: Ptr Datatype
foreign import ccall unsafe "&mpi_double" double_ :: Ptr Datatype
foreign import ccall unsafe "&mpi_long_double" longDouble_ :: Ptr Datatype
foreign import ccall unsafe "&mpi_byte" byte_ :: Ptr Datatype
foreign import ccall unsafe "&mpi_packed" packed_ :: Ptr Datatype

char, wchar, short, int, long, longLong, unsignedChar, unsignedShort :: Datatype
unsigned, unsignedLong, unsignedLongLong, float, double, longDouble :: Datatype
byte, packed :: Datatype

char = unsafePerformIO $ peek char_
wchar = unsafePerformIO $ peek wchar_
short = unsafePerformIO $ peek short_
int = unsafePerformIO $ peek int_
long = unsafePerformIO $ peek long_
longLong = unsafePerformIO $ peek longLong_
unsignedChar = unsafePerformIO $ peek unsignedChar_
unsignedShort = unsafePerformIO $ peek unsignedShort_
unsigned = unsafePerformIO $ peek unsigned_
unsignedLong = unsafePerformIO $ peek unsignedLong_
unsignedLongLong = unsafePerformIO $ peek unsignedLongLong_
float = unsafePerformIO $ peek float_
double = unsafePerformIO $ peek double_
longDouble = unsafePerformIO $ peek longDouble_
byte = unsafePerformIO $ peek byte_
packed = unsafePerformIO $ peek packed_


type Errhandler = {# type MPI_Errhandler #}
foreign import ccall "&mpi_errors_are_fatal" errorsAreFatal_ :: Ptr Errhandler
foreign import ccall "&mpi_errors_return" errorsReturn_ :: Ptr Errhandler
errorsAreFatal, errorsReturn :: Errhandler
errorsAreFatal = unsafePerformIO $ peek errorsAreFatal_
errorsReturn = unsafePerformIO $ peek errorsReturn_

{# enum ErrorClass {underscoreToCase} deriving (Eq,Ord,Show,Typeable) #}

-- XXX Should this be a ForeinPtr?
-- there is a MPI_Group_free function, which we should probably
-- call when the group is no longer referenced.

-- | Actual Haskell type used depends on the MPI implementation.
type MPIGroup = {# type MPI_Group #}

newtype Group = MkGroup { fromGroup :: MPIGroup }

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
type Operation = {# type MPI_Op #}

foreign import ccall unsafe "&mpi_max" maxOp_ :: Ptr Operation
foreign import ccall unsafe "&mpi_min" minOp_ :: Ptr Operation
foreign import ccall unsafe "&mpi_sum" sumOp_ :: Ptr Operation
foreign import ccall unsafe "&mpi_prod" prodOp_ :: Ptr Operation
foreign import ccall unsafe "&mpi_land" landOp_ :: Ptr Operation
foreign import ccall unsafe "&mpi_band" bandOp_ :: Ptr Operation
foreign import ccall unsafe "&mpi_lor" lorOp_ :: Ptr Operation
foreign import ccall unsafe "&mpi_bor" borOp_ :: Ptr Operation
foreign import ccall unsafe "&mpi_lxor" lxorOp_ :: Ptr Operation
foreign import ccall unsafe "&mpi_bxor" bxorOp_ :: Ptr Operation
-- foreign import ccall "mpi_maxloc" maxlocOp :: Operation
-- foreign import ccall "mpi_minloc" minlocOp :: Operation
-- foreign import ccall "mpi_replace" replaceOp :: Operation
-- TODO: support for those requires better support for pair datatypes

maxOp, minOp, sumOp, prodOp, landOp, bandOp, lorOp, borOp, lxorOp, bxorOp :: Operation
maxOp = unsafePerformIO $ peek maxOp_
minOp = unsafePerformIO $ peek minOp_
sumOp = unsafePerformIO $ peek sumOp_
prodOp = unsafePerformIO $ peek prodOp_
landOp = unsafePerformIO $ peek landOp_
bandOp = unsafePerformIO $ peek bandOp_
lorOp = unsafePerformIO $ peek lorOp_
borOp = unsafePerformIO $ peek borOp_
lxorOp = unsafePerformIO $ peek lxorOp_
bxorOp = unsafePerformIO $ peek bxorOp_


{-
This module provides Haskell datatype that represents values which
could be used as MPI rank designations.
-}

-- TODO: actually, this should be 32-bit int
newtype Rank = MkRank { rankId :: Int }
   deriving (Eq, Ord, Enum, Num, Integral, Real)

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
type Request = {# type MPI_Request #}

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

{#pointer *Status as StatusPtr -> Status #}

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


{-
This module provides Haskell datatype that represents values which
could be used as MPI tags.
-}

-- TODO: actually, this is a 32-bit int, and even less than that.
-- See section 8 of MPI report about extracting MPI_TAG_UB
-- and using it here
newtype Tag = MkTag { tagVal :: Int }
   deriving (Eq, Ord, Enum, Num, Integral, Real)

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
