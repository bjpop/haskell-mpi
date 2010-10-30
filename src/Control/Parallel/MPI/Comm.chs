{-# LANGUAGE ForeignFunctionInterface #-}

#include <mpi.h>
-----------------------------------------------------------------------------
{- |
Module      : Control.Parallel.MPI.Comm
Copyright   : (c) 2010 Bernie Pope, Dmitry Astapov
License     : BSD-style
Maintainer  : florbitous@gmail.com
Stability   : experimental
Portability : ghc

This module provides Haskell representation of the @MPI_Comm@ type
(communicator handle).
-}
-----------------------------------------------------------------------------
module Control.Parallel.MPI.Comm (Comm, commWorld, commSelf) where

import C2HS

{# context prefix = "MPI" #}

-- | Which Haskell type will be used as @Comm@ depends on the MPI
-- implementation that was selected during compilation. It could be
-- @CInt@, @Ptr ()@, @Ptr CInt@ or something else.
type Comm = {# type MPI_Comm #}
foreign import ccall "&mpi_comm_world" commWorld_ :: Ptr Comm
foreign import ccall "&mpi_comm_self" commSelf_ :: Ptr Comm

-- | Predefined handle for communicator that includes all running
-- processes. Similar to @MPI_Comm_world@
commWorld :: Comm
commWorld = unsafePerformIO $ peek commWorld_

-- | Predefined handle for communicator that includes only current
-- process. Similar to @MPI_Comm_self@
commSelf :: Comm
commSelf = unsafePerformIO $ peek commSelf_
