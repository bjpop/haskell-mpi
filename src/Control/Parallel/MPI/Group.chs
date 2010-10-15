{-# LANGUAGE ForeignFunctionInterface #-}

#include <mpi.h>

module Control.Parallel.MPI.Group (Group, groupEmpty) where

import C2HS

{# context prefix = "MPI" #}

-- XXX Should this be a ForeinPtr?
-- there is a MPI_Group_free function, which we should probably
-- call when the group is no longer referenced.
type Group = {# type MPI_Group #}
foreign import ccall "&mpi_group_empty" groupEmpty_ :: Ptr Group
groupEmpty :: Group
groupEmpty = unsafePerformIO $ peek groupEmpty_
