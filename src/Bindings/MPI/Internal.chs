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

{-
withStorable :: Storable a => a -> (Ptr a -> IO b) -> IO b
withStorable x f = alloca (\ptr -> poke ptr x >> f ptr)

withStorableCast :: Storable a => a -> (Ptr c -> IO b) -> IO b
withStorableCast x f = withStorable x (f . castPtr)
-}

{# fun unsafe Comm_rank as ^ { id `Comm', alloca- `Int' peekIntConv* } -> `Int' #}

-- int MPI_Send(void *buf, int count, MPI_Datatype datatype, int dest, int tag, MPI_Comm comm)
-- {# fun unsafe Send as ^ `(Storable msg, Enum dest, Enum tag)' => { withStorableCast* `msg', `Int', id `Datatype', enumToCInt `dest', enumToCInt `tag', id `Comm' } -> `Int' #}
send = {# call unsafe Send as send_ #}

-- int MPI_Recv(void *buf, int count, MPI_Datatype datatype, int source, int tag, MPI_Comm comm, MPI_Status *status)
recv = {# call unsafe Recv as recv_ #}
-- Couldn't make this work with fun because of the need to combine alloca with castPtr.
-- {# fun unsafe Recv as recv_ { id `Ptr ()', `Int', id `Datatype', `Int', `Int', id `Comm', alloca- `Status' peek* } -> `Int' #}

probe_ = {# call unsafe Probe as probe__ #}

probe :: (Enum source, Enum tag) => source -> tag -> Comm -> IO (Int, Status)
probe source tag comm = do
   let cSource = enumToCInt source
       cTag    = enumToCInt tag 
   alloca $ \statusPtr -> do
      result <- probe_ cSource cTag comm (castPtr statusPtr)
      status <- peek statusPtr
      return (cIntConv result, status)
