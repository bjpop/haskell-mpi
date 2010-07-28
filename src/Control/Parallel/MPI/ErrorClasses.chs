{-# LANGUAGE ForeignFunctionInterface, DeriveDataTypeable #-}

#include "error_classes.h"

module Control.Parallel.MPI.ErrorClasses (ErrorClass (..)) where

-- import Control.Exception (Exception (..))
import Control.Exception.Extensible
import Data.Typeable

{# context prefix = "MPI" #}

{# enum ErrorClass {underscoreToCase} deriving (Eq,Ord,Show,Typeable) #} 

instance Exception ErrorClass 
