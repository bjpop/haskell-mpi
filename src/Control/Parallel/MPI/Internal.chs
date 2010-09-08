{-# LANGUAGE ForeignFunctionInterface #-}

#include <mpi.h>
#include "init_wrapper.h"

module Control.Parallel.MPI.Internal 
   ( init, initThread, finalize, send, recv, commRank, probe, commSize,
     iSend, iRecv, bcast, barrier, wait, test
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
recv = {# call unsafe Recv as recv_ #}
iSend = {# call unsafe Isend as iSend_ #}
iRecv = {# call Irecv as iRecv_ #}
bcast = {# call unsafe Bcast as bcast_ #}
barrier = {# call unsafe Barrier as barrier_ #}
wait = {# call unsafe Wait as wait_ #}
test = {# call unsafe Test as test_ #}
