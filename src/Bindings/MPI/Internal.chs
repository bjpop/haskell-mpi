{-# LANGUAGE ForeignFunctionInterface #-}

#include <mpi.h>
#include "init_wrapper.h"
#include "constants.h"

module Bindings.MPI.Internal (init, finalize, send, recv, Comm, Datatype, commWorld, int) where

import Prelude hiding (init)
import C2HS

type Comm = {# type MPI_Comm #}
type Datatype = {# type MPI_Datatype #}

foreign import ccall "mpi_comm_world" commWorld :: Comm
foreign import ccall "mpi_int" int :: Datatype

init = {# call init_wrapper as ^ #}

-- int MPI_Finalize(void)
finalize = {# call MPI_Finalize as ^ #}

-- int MPI_Send(void *buf, int count, MPI_Datatype datatype, int dest, int tag, MPI_Comm comm)
send = {# call MPI_Send as ^ #}

-- int MPI_Recv(void *buf, int count, MPI_Datatype datatype, int source, int tag, MPI_Comm comm, MPI_Status *status)
recv = {# call MPI_Recv as ^ #}
