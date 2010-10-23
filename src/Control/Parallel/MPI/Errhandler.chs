{-# LANGUAGE ForeignFunctionInterface #-}

#include <mpi.h>

module Control.Parallel.MPI.Errhandler (Errhandler, errorsAreFatal, errorsReturn) where

import C2HS

{# context prefix = "MPI" #}

type Errhandler = {# type MPI_Errhandler #}
foreign import ccall "&mpi_errors_are_fatal" errorsAreFatal_ :: Ptr Errhandler
foreign import ccall "&mpi_errors_return" errorsReturn_ :: Ptr Errhandler
errorsAreFatal, errorsReturn :: Errhandler
errorsAreFatal = unsafePerformIO $ peek errorsAreFatal_
errorsReturn = unsafePerformIO $ peek errorsReturn_

