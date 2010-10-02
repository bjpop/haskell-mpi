{-# LANGUAGE ForeignFunctionInterface #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}

#include <mpi.h>
#include "compare.h"
#include "init_wrapper.h"

module Control.Parallel.MPI.Internal
   ( init, initThread, finalize, send, bsend, ssend, rsend, recv,
     commRank, probe, commSize, commTestInter, commRemoteSize,
     commCompare, Compare(..),
     isend, ibsend, issend, irecv, bcast, barrier, wait, test,
     cancel, scatter, gather,
     scatterv, gatherv,
     allgather, allgatherv,
     alltoall, alltoallv,
     wtime, wtick
   ) where

import Prelude hiding (init)
import C2HS

{# context prefix = "MPI" #}

{# enum Compare {underscoreToCase} deriving (Eq,Ord,Show) #}

init = {# call unsafe init_wrapper as init_wrapper_ #}
initThread = {# call unsafe init_wrapper_thread as init_wrapper_thread_ #}
finalize = {# call unsafe Finalize as finalize_ #}
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
wtime = {# call unsafe Wtime as wtime_ #}
wtick = {# call unsafe Wtick as wtick_ #}
