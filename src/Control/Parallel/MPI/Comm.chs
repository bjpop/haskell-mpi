{-# LANGUAGE ForeignFunctionInterface #-}

#include <mpi.h>
#include "comm.h"

module Control.Parallel.MPI.Comm (Comm, commWorld, CommCompare(..) ) where

import C2HS

{# context prefix = "MPI" #}

type Comm = {# type MPI_Comm #}
foreign import ccall "mpi_comm_world" commWorld :: Comm

{# enum CommCompare {underscoreToCase} deriving (Eq,Ord,Show) #}
