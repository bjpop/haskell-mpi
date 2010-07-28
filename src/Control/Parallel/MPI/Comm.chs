{-# LANGUAGE ForeignFunctionInterface #-}

#include <mpi.h>

module Control.Parallel.MPI.Comm (Comm, commWorld) where

import C2HS

{# context prefix = "MPI" #}

type Comm = {# type MPI_Comm #}
foreign import ccall "mpi_comm_world" commWorld :: Comm
