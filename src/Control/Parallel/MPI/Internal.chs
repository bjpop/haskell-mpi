{-# LANGUAGE ForeignFunctionInterface #-}

#include <mpi.h>
#include "init_wrapper.h"

module Control.Parallel.MPI.Internal
   ( init, initThread, finalize, send, bsend, ssend, rsend, recv,
     commRank, probe, commSize,
     isend, ibsend, issend, irecv, bcast, barrier, wait, test,
     cancel, scatter, gather,
     scatterv, gatherv
   ) where

import Prelude hiding (init, error)
import C2HS
import Control.Applicative ((<$>))
import Control.Parallel.MPI.Comm (Comm)
import Control.Parallel.MPI.Request (Request)
import Control.Parallel.MPI.Status (Status, StatusPtr)
import Control.Parallel.MPI.MarshalUtils (enumToCInt)
import Control.Parallel.MPI.ErrorClasses (ErrorClass)
import Control.Parallel.MPI.MarshalUtils (enumFromCInt)
import Control.Parallel.MPI.Utils (checkError)

{# context prefix = "MPI" #}

init = {# call unsafe init_wrapper as init_wrapper_ #}
initThread = {# call unsafe init_wrapper_thread as init_wrapper_thread_ #}
finalize = {# call unsafe Finalize as finalize_ #}
commSize = {# call unsafe Comm_size as commSize_ #}
commRank = {# call unsafe Comm_rank as commRank_ #}
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

