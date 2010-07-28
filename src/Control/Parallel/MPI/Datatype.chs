{-# LANGUAGE ForeignFunctionInterface #-}

#include <mpi.h>

module Control.Parallel.MPI.Datatype
   (Datatype, int, byte) where

import C2HS

{# context prefix = "MPI" #}

type Datatype = {# type MPI_Datatype #}

foreign import ccall "mpi_int" int :: Datatype
foreign import ccall "mpi_byte" byte :: Datatype
