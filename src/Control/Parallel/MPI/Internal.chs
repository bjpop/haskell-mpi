{-# LANGUAGE ForeignFunctionInterface #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}

#include <mpi.h>
#include "init_wrapper.h"

module Control.Parallel.MPI.Internal
   ( max_processor_name,
     init, initThread, queryThread, isThreadMain,
     finalize, getProcessorName,
     send, bsend, ssend, rsend, recv,
     commRank, probe, commSize, commTestInter, commRemoteSize,
     commCompare,
     isend, ibsend, issend, irecv, bcast, barrier, wait, test,
     cancel, scatter, gather,
     scatterv, gatherv,
     allgather, allgatherv,
     alltoall, alltoallv,
     reduce, allreduce, reduceScatter,
     opCreate, opFree,
     wtime, wtick,
     commGroup, groupRank, groupSize, groupUnion, groupIntersection, groupDifference,
     groupCompare, groupExcl, groupIncl, groupTranslateRanks
   ) where

import Prelude hiding (init)
import C2HS

{# context prefix = "MPI" #}

foreign import ccall "mpi_max_processor_name" max_processor_name :: Int

init = {# call unsafe init_wrapper as init_wrapper_ #}
initThread = {# call unsafe init_wrapper_thread as init_wrapper_thread_ #}
queryThread = {# call unsafe Query_thread as queryThread_ #}
isThreadMain = {# call unsafe Is_thread_main as isThreadMain_ #}
finalize = {# call unsafe Finalize as finalize_ #}
getProcessorName = {# call unsafe Get_processor_name as getProcessorName_ #}
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
groupRank = {# call unsafe Group_rank as groupRank_ #}
groupSize = {# call unsafe Group_size as groupSize_ #}
groupUnion = {# call unsafe Group_union as groupUnion_ #}
groupIntersection = {# call unsafe Group_intersection as groupIntersection_ #}
groupDifference = {# call unsafe Group_difference as groupDifference_ #}
groupCompare = {# call unsafe Group_compare as groupCompare_ #}
groupExcl = {# call unsafe Group_excl as groupExcl_ #}
groupIncl = {# call unsafe Group_incl as groupIncl_ #}
groupTranslateRanks = {# call unsafe Group_translate_ranks as groupTranslateRanks_ #}
