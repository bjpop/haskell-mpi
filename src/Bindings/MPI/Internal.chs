{-# LANGUAGE ForeignFunctionInterface #-}

#include <mpi.h>
#include "init_wrapper.h"

module Bindings.MPI.Internal 
   (init, finalize, send, recv, commRank, probe) where

import Prelude hiding (init, error)
import C2HS
import Bindings.MPI.Comm (Comm)
import Bindings.MPI.Status (Status, StatusPtr)
import Bindings.MPI.MarshalUtils (enumToCInt)

{# context prefix = "MPI" #}

{# fun unsafe init_wrapper as init {} -> `Int' #}

{# fun unsafe Finalize as finalize {} -> `Int' #}

-- {# fun unsafe Comm_rank as ^ { id `Comm', alloca- `Int' peekIntConv* } -> `Int' #}
-- {# fun unsafe Comm_rank as ^ `Enum rank' => { id `Comm', alloca- `Int' peekEnum* } -> `rank' id #}

commRank = {# call unsafe Comm_rank as commRank_ #}

send = {# call unsafe Send as send_ #}

recv = {# call unsafe Recv as recv_ #}

probe_ = {# call unsafe Probe as probe__ #}

probe :: (Enum source, Enum tag) => source -> tag -> Comm -> IO (Int, Status)
probe source tag comm = do
   let cSource = enumToCInt source
       cTag    = enumToCInt tag 
   alloca $ \statusPtr -> do
      result <- probe_ cSource cTag comm (castPtr statusPtr)
      status <- peek statusPtr
      return (cIntConv result, status)
