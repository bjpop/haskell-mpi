{-# LANGUAGE ForeignFunctionInterface, DeriveDataTypeable #-}

#include "thread_support.h"

module Control.Parallel.MPI.ThreadSupport (ThreadSupport (..)) where

{# context prefix = "MPI" #}

{# enum ThreadSupport {underscoreToCase} deriving (Eq,Ord,Show) #} 
