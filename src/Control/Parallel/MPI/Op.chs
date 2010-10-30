{-# LANGUAGE ForeignFunctionInterface #-}

#include <mpi.h>
-----------------------------------------------------------------------------
{- |
Module      : Control.Parallel.MPI.Op
Copyright   : (c) 2010 Bernie Pope, Dmitry Astapov
License     : BSD-style
Maintainer  : florbitous@gmail.com
Stability   : experimental
Portability : ghc

This module provides Haskell type that represents values of @MPI_Op@
type (reduction operations), and predefined reduction operations
defined in the MPI Report.
-}
-----------------------------------------------------------------------------
module Control.Parallel.MPI.Op
   (Operation, maxOp, minOp, sumOp, prodOp, landOp, bandOp, lorOp,
   borOp, lxorOp, bxorOp) where

import C2HS

{# context prefix = "MPI" #}

-- | Actual Haskell type used depends on the MPI implementation.
type Operation = {# type MPI_Op #}

foreign import ccall unsafe "&mpi_max" maxOp_ :: Ptr Operation
foreign import ccall unsafe "&mpi_min" minOp_ :: Ptr Operation
foreign import ccall unsafe "&mpi_sum" sumOp_ :: Ptr Operation
foreign import ccall unsafe "&mpi_prod" prodOp_ :: Ptr Operation
foreign import ccall unsafe "&mpi_land" landOp_ :: Ptr Operation
foreign import ccall unsafe "&mpi_band" bandOp_ :: Ptr Operation
foreign import ccall unsafe "&mpi_lor" lorOp_ :: Ptr Operation
foreign import ccall unsafe "&mpi_bor" borOp_ :: Ptr Operation
foreign import ccall unsafe "&mpi_lxor" lxorOp_ :: Ptr Operation
foreign import ccall unsafe "&mpi_bxor" bxorOp_ :: Ptr Operation
-- foreign import ccall "mpi_maxloc" maxlocOp :: Operation
-- foreign import ccall "mpi_minloc" minlocOp :: Operation
-- foreign import ccall "mpi_replace" replaceOp :: Operation
-- TODO: support for those requires better support for pair datatypes

maxOp, minOp, sumOp, prodOp, landOp, bandOp, lorOp, borOp, lxorOp, bxorOp :: Operation
maxOp = unsafePerformIO $ peek maxOp_
minOp = unsafePerformIO $ peek minOp_
sumOp = unsafePerformIO $ peek sumOp_
prodOp = unsafePerformIO $ peek prodOp_
landOp = unsafePerformIO $ peek landOp_
bandOp = unsafePerformIO $ peek bandOp_
lorOp = unsafePerformIO $ peek lorOp_
borOp = unsafePerformIO $ peek borOp_
lxorOp = unsafePerformIO $ peek lxorOp_
bxorOp = unsafePerformIO $ peek bxorOp_
