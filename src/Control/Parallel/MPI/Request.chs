{-# LANGUAGE ForeignFunctionInterface #-}

#include <mpi.h>

module Control.Parallel.MPI.Request (Request) where

import C2HS

{# context prefix = "MPI" #}

type Request = {# type MPI_Request #}
