{-# LANGUAGE ForeignFunctionInterface #-}

#include <mpi.h>

module Control.Parallel.MPI.Group (Group) where

import C2HS

{# context prefix = "MPI" #}

-- XXX Should this be a ForeinPtr?
-- there is a MPI_Group_free function, which we should probably
-- call when the group is no longer referenced.
type Group = {# type MPI_Group #}
-- foreign import ccall "mpi_comm_world" commWorld :: Comm
