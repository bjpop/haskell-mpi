{-# LANGUAGE ForeignFunctionInterface #-}

#include <mpi.h>

module Control.Parallel.MPI.Op
   (Operation, maxOp, minOp, sumOp, prodOp, landOp, bandOp, lorOp,
   borOp, lxorOp, bxorOp) where

import C2HS

{# context prefix = "MPI" #}

type Operation = {# type MPI_Op #}

foreign import ccall "mpi_max" maxOp :: Operation
foreign import ccall "mpi_min" minOp :: Operation
foreign import ccall "mpi_sum" sumOp :: Operation
foreign import ccall "mpi_prod" prodOp :: Operation
foreign import ccall "mpi_land" landOp :: Operation
foreign import ccall "mpi_band" bandOp :: Operation
foreign import ccall "mpi_lor" lorOp :: Operation
foreign import ccall "mpi_bor" borOp :: Operation
foreign import ccall "mpi_lxor" lxorOp :: Operation
foreign import ccall "mpi_bxor" bxorOp :: Operation
-- foreign import ccall "mpi_maxloc" maxlocOp :: Operation
-- foreign import ccall "mpi_minloc" minlocOp :: Operation
-- foreign import ccall "mpi_replace" replaceOp :: Operation
