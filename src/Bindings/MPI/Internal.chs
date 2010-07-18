{-# LANGUAGE ForeignFunctionInterface #-}

#include <mpi.h>
#include "init_wrapper.h"

module Bindings.MPI.Internal 
   ( init, finalize, send, recv, commRank, probe, commSize,
     iSend, iRecv 
   ) where

import Prelude hiding (init, error)
import C2HS
import Control.Applicative ((<$>))
import Bindings.MPI.Comm (Comm)
import Bindings.MPI.Request (Request)
import Bindings.MPI.Status (Status, StatusPtr)
import Bindings.MPI.MarshalUtils (enumToCInt)
import Bindings.MPI.ErrorClasses (ErrorClass)
import Bindings.MPI.MarshalUtils (enumFromCInt)
import Bindings.MPI.Utils (checkError)

{# context prefix = "MPI" #}

init = {# call unsafe init_wrapper as init_wrapper_ #}
finalize = {# call unsafe Finalize as finalize_ #}
commSize = {# call unsafe Comm_size as commSize_ #}
commRank = {# call unsafe Comm_rank as commRank_ #}
probe = {# call Probe as probe_ #}
send = {# call unsafe Send as send_ #}
recv = {# call unsafe Recv as recv_ #}
iSend = {# call unsafe Isend as iSend_ #}
iRecv = {# call Irecv as iRecv_ #}
