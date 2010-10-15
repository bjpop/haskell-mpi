{-# LANGUAGE ForeignFunctionInterface #-}

#include <mpi.h>

module Control.Parallel.MPI.Comm (Comm, commWorld, commSelf) where

import C2HS

{# context prefix = "MPI" #}

type Comm = {# type MPI_Comm #}
foreign import ccall "&mpi_comm_world" commWorld_ :: Ptr Comm
foreign import ccall "&mpi_comm_self" commSelf_ :: Ptr Comm

commWorld, commSelf :: Comm
commWorld = unsafePerformIO $ peek commWorld_
commSelf = unsafePerformIO $ peek commSelf_
