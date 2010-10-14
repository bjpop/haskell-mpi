{-# LANGUAGE ScopedTypeVariables, FlexibleContexts #-}

module PrimTypeTests (primTypeTests) where

import TestHelpers
import Control.Parallel.MPI.Storable
import C2HS

primTypeTests :: Rank -> [(String,TestRunnerTest)]
primTypeTests rank =
  [ mpiTestCase rank "Int maxBound" (sendRecvSingleValTest (maxBound :: Int))
  , mpiTestCase rank "Int minBound" (sendRecvSingleValTest (minBound :: Int))
  , mpiTestCase rank "Int32 maxBound" (sendRecvSingleValTest (maxBound :: Int32))
  , mpiTestCase rank "Int32 minBound" (sendRecvSingleValTest (minBound :: Int32))    
  , mpiTestCase rank "Int64 maxBound" (sendRecvSingleValTest (maxBound :: Int64))
  , mpiTestCase rank "Int64 minBound" (sendRecvSingleValTest (minBound :: Int64))
  , mpiTestCase rank "Word maxBound" (sendRecvSingleValTest (maxBound :: Word))
  , mpiTestCase rank "Word minBound" (sendRecvSingleValTest (minBound :: Word))
  , mpiTestCase rank "Word32 maxBound" (sendRecvSingleValTest (maxBound :: Word32))
  , mpiTestCase rank "Word32 minBound" (sendRecvSingleValTest (minBound :: Word32))    
  , mpiTestCase rank "Word64 maxBound" (sendRecvSingleValTest (maxBound :: Word64))
  , mpiTestCase rank "Word64 minBound" (sendRecvSingleValTest (minBound :: Int64))
  , mpiTestCase rank "checking sizes of Haskell types vs MPI representations" reprSizeTest
  ]

sendRecvSingleValTest :: forall a . (RecvInto (Ptr a), Repr a, SendFrom a, Storable a, Eq a, Show a) => a -> Rank -> IO ()
sendRecvSingleValTest val rank 
  | rank == 0 = send commWorld 1 unitTag (val :: a)
  | rank == 1 = do
    (result :: a, _status) <- intoNewVal $ recv commWorld 0 unitTag
    result == val @? "result: " ++ show result ++ " not equal to sent val: " ++ show (val :: a)
  | otherwise = return ()

reprSizeTest :: Rank -> IO ()
reprSizeTest _ = do
  check "Bool"   (undefined :: Bool)
  check "Char"   (undefined :: Char)
  check "CChar"  (undefined :: CChar)
  check "Int"    (undefined :: Int)
  check "CInt"   (undefined :: CInt)
  check "Int8"  (undefined :: Int8)
  check "Int16"  (undefined :: Int16)
  check "Int32"  (undefined :: Int32)
  check "Int64"  (undefined :: Int64)
  check "Word"   (undefined :: Word)
  check "Word8"  (undefined :: Word8)
  check "Word16" (undefined :: Word16)
  check "Word32" (undefined :: Word32)
  check "Word64" (undefined :: Word64)
  check "Double" (undefined :: Double)
  check "Float"  (undefined :: Float)
  where
    check tName hType = do
      let hSize = sizeOf hType 
          (scale, mType) = representation hType
          mSize = typeSize (mType) * scale
      
      hSize == mSize @? tName ++ " has size of " ++ show hSize ++ ", but its MPI representation has size of " ++ show mSize