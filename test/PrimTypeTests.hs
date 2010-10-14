{-# LANGUAGE ScopedTypeVariables, FlexibleContexts #-}

module PrimTypeTests (primTypeTests) where

import TestHelpers
import Control.Parallel.MPI.Storable
import C2HS
import Data.Typeable

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
  , mpiTestCase rank "intSize" (sizeSingleValTest (undefined :: Int))
  , mpiTestCase rank "int8Size" (sizeSingleValTest (undefined :: Int8))
  , mpiTestCase rank "int16Size" (sizeSingleValTest (undefined :: Int16))
  , mpiTestCase rank "int32Size" (sizeSingleValTest (undefined :: Int32))
  , mpiTestCase rank "int64Size" (sizeSingleValTest (undefined :: Int64))
  , mpiTestCase rank "charSize" (sizeSingleValTest (undefined :: Char))
  , mpiTestCase rank "boolSize" (sizeSingleValTest (undefined :: Bool))
  , mpiTestCase rank "floatSize" (sizeSingleValTest (undefined :: Float))
  , mpiTestCase rank "doubleSize" (sizeSingleValTest (undefined :: Double))
  , mpiTestCase rank "CIntSize" (sizeSingleValTest (undefined :: CInt))
  , mpiTestCase rank "CCharSize" (sizeSingleValTest (undefined :: CChar))
  ]

sendRecvSingleValTest :: forall a . (Typeable a, RecvInto (Ptr a), Repr a, SendFrom a, Storable a, Eq a, Show a) => a -> Rank -> IO ()
sendRecvSingleValTest val rank =
   if rank == 0
      then send commWorld 1 unitTag (val :: a)
      else do
         (result :: a, _status) <- intoNewVal $ recv commWorld 0 unitTag
         result == val @? "result: " ++ show result ++ " not equal to sent val: " ++ show (val :: a) ++ " for type " ++ show (typeOf val)

sizeSingleValTest :: (Typeable a, Storable a, Show a, Eq a, Repr a) => a -> Rank -> IO ()
sizeSingleValTest val _rank = do
   let (_,mpiType) = representation val
       mpiTypeSize = typeSize mpiType
       storableSize = sizeOf val
   mpiTypeSize == storableSize @? "MPI repr type size: " ++ show mpiTypeSize ++ " not equal to storable size: " ++ show storableSize ++ " for type " ++ show (typeOf val)
