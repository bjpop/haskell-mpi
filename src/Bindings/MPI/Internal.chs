{-# LANGUAGE ForeignFunctionInterface #-}

#include <mpi.h>
#include "init_wrapper.h"

module Bindings.MPI.Internal 
   (init, finalize, send, recv, commRank, probe) where

import Prelude hiding (init, error)
import C2HS
import Control.Applicative ((<$>))
import Bindings.MPI.Comm (Comm)
import Bindings.MPI.Status (Status, StatusPtr)
import Bindings.MPI.MarshalUtils (enumToCInt)
import Bindings.MPI.ErrorClasses (ErrorClass)
import Bindings.MPI.MarshalUtils (enumFromCInt)
import Bindings.MPI.Utils (checkError)

{# context prefix = "MPI" #}

-- {# fun unsafe init_wrapper as init {} -> `ErrorClass' enumFromCInt #}
init = {# call unsafe init_wrapper as init_wrapper_ #}

-- {# fun unsafe Finalize as finalize {} -> `ErrorClass' enumFromCInt #}
finalize = {# call unsafe Finalize as finalize_ #}

commRank = {# call unsafe Comm_rank as commRank_ #}

send = {# call unsafe Send as send_ #}

recv = {# call unsafe Recv as recv_ #}

probe = {# call unsafe Probe as probe_ #}
