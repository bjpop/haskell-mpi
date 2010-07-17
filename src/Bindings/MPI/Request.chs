{-# LANGUAGE ForeignFunctionInterface #-}

#include <mpi.h>

module Bindings.MPI.Request (Request) where

import C2HS

{# context prefix = "MPI" #}

type Request = {# type MPI_Request #}
