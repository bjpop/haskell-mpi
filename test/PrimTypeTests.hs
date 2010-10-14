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

reprSizeTest _ = do
  sizeOf (undefined :: Int) == (typeSize int) @? "Size of Int differs from size of MPI_INT"