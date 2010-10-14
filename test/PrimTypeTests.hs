{-# LANGUAGE ScopedTypeVariables, FlexibleContexts #-}

module PrimTypeTests (primTypeTests) where

import TestHelpers
import Control.Parallel.MPI.Storable
import C2HS

primTypeTests :: Rank -> [(String,TestRunnerTest)]
primTypeTests rank =
  [ mpiTestCase rank "intMaxBound" (sendRecvSingleValTest (maxBound :: Int))
  , mpiTestCase rank "intMinBound" (sendRecvSingleValTest (minBound :: Int))
  ]

sendRecvSingleValTest :: forall a . (RecvInto (Ptr a), Repr a, SendFrom a, Storable a, Eq a, Show a) => a -> Rank -> IO ()
sendRecvSingleValTest val rank =
   if rank == 0
      then send commWorld 1 unitTag (val :: a)
      else do
         (result :: a, _status) <- intoNewVal $ recv commWorld 0 unitTag
         result == val @? "result: " ++ show result ++ " not equal to sent val: " ++ show (val :: a)
