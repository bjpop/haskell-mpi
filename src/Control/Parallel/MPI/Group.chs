{-# LANGUAGE ForeignFunctionInterface #-}

#include <mpi.h>
-----------------------------------------------------------------------------
{- |
Module      : Control.Parallel.MPI.Group
Copyright   : (c) 2010 Bernie Pope, Dmitry Astapov
License     : BSD-style
Maintainer  : florbitous@gmail.com
Stability   : experimental
Portability : ghc

This module provides Haskell datatype that represents MPI process group
handles (@MPI_Group@).
-}
-----------------------------------------------------------------------------
module Control.Parallel.MPI.Group (Group, groupEmpty) where

import C2HS

{# context prefix = "MPI" #}

-- XXX Should this be a ForeinPtr?
-- there is a MPI_Group_free function, which we should probably
-- call when the group is no longer referenced.

-- | Actual Haskell type used depends on the MPI implementation.
type Group = {# type MPI_Group #}

foreign import ccall "&mpi_group_empty" groupEmpty_ :: Ptr Group
-- | Predefined handle for group without any members. Corresponds to @MPI_GROUP_EMPTY@
groupEmpty :: Group
groupEmpty = unsafePerformIO $ peek groupEmpty_
