{-# LANGUAGE ForeignFunctionInterface, DeriveDataTypeable #-}

#include "thread_support.h"
-----------------------------------------------------------------------------
{- |
Module      : Control.Parallel.MPI.ThreadSupport
Copyright   : (c) 2010 Bernie Pope, Dmitry Astapov
License     : BSD-style
Maintainer  : florbitous@gmail.com
Stability   : experimental
Portability : ghc

This module provides Haskell datatypes that comprises of values of
predefined MPI constants @MPI_THREAD_SINGLE@, @MPI_THREAD_FUNNELED@,
@MPI_THREAD_SERIALIZED@, @MPI_THREAD_MULTIPLE@.
-}
-----------------------------------------------------------------------------
module Control.Parallel.MPI.ThreadSupport (ThreadSupport (..)) where

{# context prefix = "MPI" #}

{# enum ThreadSupport {underscoreToCase} deriving (Eq,Ord,Show) #} 
