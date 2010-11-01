{-# LANGUAGE ScopedTypeVariables #-}
module Main where

import Control.Parallel.MPI.Storable
import Control.Parallel.MPI
import Data.Array.Storable
import Foreign.C.Types (CInt)
import Control.Monad (forM_)

type Msg = StorableArray Int CInt
bounds :: (Int, Int)
bounds@(lo,hi) = (1,100)
tag :: Tag
tag = 0

main :: IO ()
main = mpiWorld $ \size rank -> do
   putStrLn $ "Haskell process with rank " ++ show rank ++ " world with size " ++ show size
   if rank == 1
      then do
         (msg :: Msg) <- newArray bounds 0
         _status <- recv commWorld 0 tag msg
         forM_ [lo .. hi] $ \i -> do
            val <- readArray msg i
            writeArray msg i (val*val)
         send commWorld 0 tag msg
      else
         putStrLn "This program must be rank 1"
