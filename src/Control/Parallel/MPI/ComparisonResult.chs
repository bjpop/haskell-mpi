{-# LANGUAGE ForeignFunctionInterface #-}

#include "comparison_result.h"

module Control.Parallel.MPI.ComparisonResult (ComparisonResult (..)) where

{# context prefix = "MPI" #}

{# enum ComparisonResult {underscoreToCase} deriving (Eq,Ord,Show) #}
