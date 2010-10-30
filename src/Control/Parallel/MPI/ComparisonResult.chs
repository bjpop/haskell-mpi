{-# LANGUAGE ForeignFunctionInterface #-}

#include "comparison_result.h"

-----------------------------------------------------------------------------
{- |
Module      : Control.Parallel.MPI.ComparisonResult
Copyright   : (c) 2010 Bernie Pope, Dmitry Astapov
License     : BSD-style
Maintainer  : florbitous@gmail.com
Stability   : experimental
Portability : ghc

This module provides Haskell enum that comprises of MPI constants
@MPI_IDENT@, @MPI_CONGRUENT@, @MPI_SIMILAR@ and @MPI_UNEQUAL@.

Those are used to compare communicators (f.e. 'commCompare') and
process groups (f.e. 'groupCompare').
-}
-----------------------------------------------------------------------------
module Control.Parallel.MPI.ComparisonResult (ComparisonResult (..)) where

{# context prefix = "MPI" #}

{# enum ComparisonResult {underscoreToCase} deriving (Eq,Ord,Show) #}
