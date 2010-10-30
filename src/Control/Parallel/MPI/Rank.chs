{-# LANGUAGE ForeignFunctionInterface, GeneralizedNewtypeDeriving #-}

-----------------------------------------------------------------------------
{- |
Module      : Control.Parallel.MPI.Request
Copyright   : (c) 2010 Bernie Pope, Dmitry Astapov
License     : BSD-style
Maintainer  : florbitous@gmail.com
Stability   : experimental
Portability : ghc

This module provides Haskell datatype that represents values which
could be used as MPI rank designations.
-}
-----------------------------------------------------------------------------
module Control.Parallel.MPI.Rank ( Rank, rankId,
  toRank, fromRank,
  anySource,
  theRoot, procNull ) where

import Foreign

-- TODO: actually, this should be 32-bit int
newtype Rank = Rank { rankId :: Int }
   deriving (Eq, Ord, Enum, Num, Integral, Real)

foreign import ccall "mpi_any_source" anySource_ :: Ptr Int
foreign import ccall "mpi_root" theRoot_ :: Ptr Int
foreign import ccall "mpi_proc_null" procNull_ :: Ptr Int

-- | Predefined rank number that allows reception of point-to-point messages
-- regardless of their source. Corresponds to @MPI_ANY_SOURCE@
anySource :: Rank
anySource = toRank $ unsafePerformIO $ peek anySource_

-- | Predefined rank number that specifies root process during
-- operations involving intercommunicators. Corresponds to @MPI_ROOT@
theRoot :: Rank
theRoot   = toRank $ unsafePerformIO $ peek theRoot_

-- | Predefined rank number that specifies non-root processes during
-- operations involving intercommunicators. Corresponds to @MPI_PROC_NULL@
procNull :: Rank
procNull  = toRank $ unsafePerformIO $ peek procNull_

instance Show Rank where
   show = show . rankId

toRank :: Enum a => a -> Rank
toRank x = Rank { rankId = fromEnum x }

fromRank :: Enum a => Rank -> a
fromRank = toEnum . rankId
