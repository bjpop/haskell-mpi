{-# LANGUAGE ForeignFunctionInterface #-}

#include <mpi.h>

module Control.Parallel.MPI.Datatype
   (Datatype, charPtr, wcharPtr, shortPtr, intPtr, longPtr, longLongPtr, unsignedCharPtr,
    unsignedShortPtr, unsignedPtr, unsignedLongPtr, unsignedLongLongPtr, floatPtr, doublePtr,
    longDoublePtr, bytePtr, packedPtr) where

import C2HS

{# context prefix = "MPI" #}

type Datatype = {# type MPI_Datatype #}

foreign import ccall "&mpi_char" charPtr :: Ptr Datatype
foreign import ccall "&mpi_wchar" wcharPtr :: Ptr Datatype
foreign import ccall "&mpi_short" shortPtr :: Ptr Datatype
foreign import ccall "&mpi_int" intPtr :: Ptr Datatype
foreign import ccall "&mpi_long" longPtr :: Ptr Datatype
foreign import ccall "&mpi_long_long" longLongPtr :: Ptr Datatype
foreign import ccall "&mpi_unsigned_char" unsignedCharPtr :: Ptr Datatype
foreign import ccall "&mpi_unsigned_short" unsignedShortPtr :: Ptr Datatype
foreign import ccall "&mpi_unsigned" unsignedPtr :: Ptr Datatype
foreign import ccall "&mpi_unsigned_long" unsignedLongPtr :: Ptr Datatype
foreign import ccall "&mpi_unsigned_long_long" unsignedLongLongPtr :: Ptr Datatype
foreign import ccall "&mpi_float" floatPtr :: Ptr Datatype
foreign import ccall "&mpi_double" doublePtr :: Ptr Datatype
foreign import ccall "&mpi_long_double" longDoublePtr :: Ptr Datatype
foreign import ccall "&mpi_byte" bytePtr :: Ptr Datatype
foreign import ccall "&mpi_packed" packedPtr :: Ptr Datatype
