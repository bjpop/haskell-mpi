{-# LANGUAGE ForeignFunctionInterface #-}

#include <mpi.h>

module Control.Parallel.MPI.Datatype
   (Datatype, char, short, int, long, longLong, unsignedChar,
    unsignedShort, unsigned, unsignedLong, float, double,
    longDouble, byte, packed) where

import C2HS

{# context prefix = "MPI" #}

type Datatype = {# type MPI_Datatype #}

foreign import ccall "mpi_char" char :: Datatype
foreign import ccall "mpi_short" short :: Datatype
foreign import ccall "mpi_int" int :: Datatype
foreign import ccall "mpi_long" long :: Datatype
foreign import ccall "mpi_long_long" longLong :: Datatype
foreign import ccall "mpi_unsigned_char" unsignedChar :: Datatype
foreign import ccall "mpi_unsigned_short" unsignedShort :: Datatype
foreign import ccall "mpi_unsigned" unsigned :: Datatype
foreign import ccall "mpi_unsigned_long" unsignedLong :: Datatype
foreign import ccall "mpi_float" float :: Datatype
foreign import ccall "mpi_double" double :: Datatype
foreign import ccall "mpi_long_double" longDouble :: Datatype
foreign import ccall "mpi_byte" byte :: Datatype
foreign import ccall "mpi_packed" packed :: Datatype
