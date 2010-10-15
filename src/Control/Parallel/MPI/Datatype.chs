{-# LANGUAGE ForeignFunctionInterface #-}

#include <mpi.h>

module Control.Parallel.MPI.Datatype
   (Datatype, char, wchar, short, int, long, longLong, unsignedChar,
    unsignedShort, unsigned, unsignedLong, unsignedLongLong, float, double,
    longDouble, byte, packed) where

import C2HS

{# context prefix = "MPI" #}

type Datatype = {# type MPI_Datatype #}

foreign import ccall unsafe "&mpi_char" char_ :: Ptr Datatype
foreign import ccall unsafe "&mpi_wchar" wchar_ :: Ptr Datatype
foreign import ccall unsafe "&mpi_short" short_ :: Ptr Datatype
foreign import ccall unsafe "&mpi_int" int_ :: Ptr Datatype
foreign import ccall unsafe "&mpi_long" long_ :: Ptr Datatype
foreign import ccall unsafe "&mpi_long_long" longLong_ :: Ptr Datatype
foreign import ccall unsafe "&mpi_unsigned_char" unsignedChar_ :: Ptr Datatype
foreign import ccall unsafe "&mpi_unsigned_short" unsignedShort_ :: Ptr Datatype
foreign import ccall unsafe "&mpi_unsigned" unsigned_ :: Ptr Datatype
foreign import ccall unsafe "&mpi_unsigned_long" unsignedLong_ :: Ptr Datatype
foreign import ccall unsafe "&mpi_unsigned_long_long" unsignedLongLong_ :: Ptr Datatype
foreign import ccall unsafe "&mpi_float" float_ :: Ptr Datatype
foreign import ccall unsafe "&mpi_double" double_ :: Ptr Datatype
foreign import ccall unsafe "&mpi_long_double" longDouble_ :: Ptr Datatype
foreign import ccall unsafe "&mpi_byte" byte_ :: Ptr Datatype
foreign import ccall unsafe "&mpi_packed" packed_ :: Ptr Datatype

char, wchar, short, int, long, longLong, unsignedChar, unsignedShort :: Datatype
unsigned, unsignedLong, unsignedLongLong, float, double, longDouble :: Datatype
byte, packed :: Datatype

char = unsafePerformIO $ peek char_
wchar = unsafePerformIO $ peek wchar_
short = unsafePerformIO $ peek short_
int = unsafePerformIO $ peek int_
long = unsafePerformIO $ peek long_
longLong = unsafePerformIO $ peek longLong_
unsignedChar = unsafePerformIO $ peek unsignedChar_
unsignedShort = unsafePerformIO $ peek unsignedShort_
unsigned = unsafePerformIO $ peek unsigned_
unsignedLong = unsafePerformIO $ peek unsignedLong_
unsignedLongLong = unsafePerformIO $ peek unsignedLongLong_
float = unsafePerformIO $ peek float_
double = unsafePerformIO $ peek double_
longDouble = unsafePerformIO $ peek longDouble_
byte = unsafePerformIO $ peek byte_
packed = unsafePerformIO $ peek packed_
