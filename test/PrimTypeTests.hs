{-# LANGUAGE ScopedTypeVariables, FlexibleContexts #-}

module PrimTypeTests (primTypeTests) where

import TestHelpers
import Control.Parallel.MPI.Fast
import Data.Typeable
import Foreign
import Foreign.C.Types

primTypeTests :: Rank -> [(String,TestRunnerTest)]
primTypeTests rank =
  [ mpiTestCase rank "intMaxBound" (sendRecvSingleValTest (maxBound :: Int))
  , mpiTestCase rank "intMinBound" (sendRecvSingleValTest (minBound :: Int))
  , mpiTestCase rank "boolMaxBound" (sendRecvSingleValTest (maxBound :: Bool))
  , mpiTestCase rank "boolMinBound" (sendRecvSingleValTest (minBound :: Bool))
  , mpiTestCase rank "charMaxBound" (sendRecvSingleValTest (maxBound :: Char))
  , mpiTestCase rank "charMinBound" (sendRecvSingleValTest (minBound :: Char))
  , mpiTestCase rank "int8MaxBound" (sendRecvSingleValTest (maxBound :: Int8))
  , mpiTestCase rank "int8MinBound" (sendRecvSingleValTest (minBound :: Int8))
  , mpiTestCase rank "int16MaxBound" (sendRecvSingleValTest (maxBound :: Int16))
  , mpiTestCase rank "int16MinBound" (sendRecvSingleValTest (minBound :: Int16))
  , mpiTestCase rank "int32MaxBound" (sendRecvSingleValTest (maxBound :: Int32))
  , mpiTestCase rank "int32MinBound" (sendRecvSingleValTest (minBound :: Int32))
  , mpiTestCase rank "int64MaxBound" (sendRecvSingleValTest (maxBound :: Int64))
  , mpiTestCase rank "int64MinBound" (sendRecvSingleValTest (minBound :: Int64))
  , mpiTestCase rank "wordMaxBound" (sendRecvSingleValTest (maxBound :: Word))
  , mpiTestCase rank "wordMinBound" (sendRecvSingleValTest (minBound :: Word))
  , mpiTestCase rank "word8MaxBound" (sendRecvSingleValTest (maxBound :: Word8))
  , mpiTestCase rank "word8MinBound" (sendRecvSingleValTest (minBound :: Word8))
  , mpiTestCase rank "word16MaxBound" (sendRecvSingleValTest (maxBound :: Word16))
  , mpiTestCase rank "word16MinBound" (sendRecvSingleValTest (minBound :: Word16))
  , mpiTestCase rank "word32MaxBound" (sendRecvSingleValTest (maxBound :: Word32))
  , mpiTestCase rank "word32MinBound" (sendRecvSingleValTest (minBound :: Word32))
  , mpiTestCase rank "word64MaxBound" (sendRecvSingleValTest (maxBound :: Word64))
  , mpiTestCase rank "word64MinBound" (sendRecvSingleValTest (minBound :: Word64))
  , mpiTestCase rank "intSize" (sizeSingleValTest (undefined :: Int))
  , mpiTestCase rank "int8Size" (sizeSingleValTest (undefined :: Int8))
  , mpiTestCase rank "int16Size" (sizeSingleValTest (undefined :: Int16))
  , mpiTestCase rank "int32Size" (sizeSingleValTest (undefined :: Int32))
  , mpiTestCase rank "int64Size" (sizeSingleValTest (undefined :: Int64))
  , mpiTestCase rank "wordSize" (sizeSingleValTest (undefined :: Word))
  , mpiTestCase rank "word8Size" (sizeSingleValTest (undefined :: Word8))
  , mpiTestCase rank "word16Size" (sizeSingleValTest (undefined :: Word16))
  , mpiTestCase rank "word32Size" (sizeSingleValTest (undefined :: Word32))
  , mpiTestCase rank "word64Size" (sizeSingleValTest (undefined :: Word64))
  , mpiTestCase rank "charSize" (sizeSingleValTest (undefined :: Char))
  , mpiTestCase rank "boolSize" (sizeSingleValTest (undefined :: Bool))
  , mpiTestCase rank "floatSize" (sizeSingleValTest (undefined :: Float))
  , mpiTestCase rank "doubleSize" (sizeSingleValTest (undefined :: Double))
  , mpiTestCase rank "CIntSize" (sizeSingleValTest (undefined :: CInt))
  , mpiTestCase rank "CCharSize" (sizeSingleValTest (undefined :: CChar))
  ]

sendRecvSingleValTest :: forall a . (Typeable a, RecvInto (Ptr a), Repr a, SendFrom a, Storable a, Eq a, Show a) => a -> Rank -> IO ()
sendRecvSingleValTest val rank
  | rank == 0 = send commWorld 1 unitTag (val :: a)
  | rank == 1 = do
    (result :: a, _status) <- intoNewVal $ recv commWorld 0 unitTag
    result == val @? "result: " ++ show result ++ " not equal to sent val: " ++ show (val :: a) ++ " for type " ++ show (typeOf val)
  | otherwise = return ()

sizeSingleValTest :: (Typeable a, Storable a, Show a, Eq a, Repr a) => a -> Rank -> IO ()
sizeSingleValTest val _rank = do
   let (scale,mpiType) = representation val
       mpiTypeSize = (typeSize mpiType) * scale
       storableSize = sizeOf val
   mpiTypeSize == storableSize @? "MPI repr type size: " ++ show mpiTypeSize ++ " not equal to storable size: " ++ show storableSize ++ " for type " ++ show (typeOf val)
