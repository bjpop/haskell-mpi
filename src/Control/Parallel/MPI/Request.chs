{-# LANGUAGE ForeignFunctionInterface #-}

#include <mpi.h>
-----------------------------------------------------------------------------
{- |
Module      : Control.Parallel.MPI.Request
Copyright   : (c) 2010 Bernie Pope, Dmitry Astapov
License     : BSD-style
Maintainer  : florbitous@gmail.com
Stability   : experimental
Portability : ghc

This module provides Haskell representation of the @MPI_Request@ type.
-}
-----------------------------------------------------------------------------
module Control.Parallel.MPI.Request (Request) where

import C2HS

{# context prefix = "MPI" #}

-- | Actuall Haskell type used depends on the MPI implementation
-- selected during compilation
type Request = {# type MPI_Request #}
