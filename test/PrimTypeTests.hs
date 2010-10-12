{-# LANGUAGE ScopedTypeVariables #-}

module PrimTypeTests (primTypeTests) where

import TestHelpers
import Control.Parallel.MPI.Storable

primTypeTests :: Rank -> [(String,TestRunnerTest)]
primTypeTests rank =
  [ mpiTestCase rank "intMaxBound" intMaxBoundTest
  ]

intMaxBoundTest :: Rank -> IO ()
intMaxBoundTest rank =
   if rank == 0
      then send commWorld 1 unitTag (maxBound :: Int)
      else do
         (result :: Int, _status) <- intoNewVal $ recv commWorld 0 unitTag
         result == maxBound @? "result: " ++ show result ++ " not equal to maxBound: " ++ show (maxBound :: Int)
