{-# LANGUAGE ScopedTypeVariables #-}
module Main where

import Control.Parallel.MPI.Storable
import Control.Parallel.MPI
import Data.Array.Storable
import Foreign.C.Types (CInt)

type Msg = StorableArray Int CInt
bounds :: (Int, Int)
bounds@(lo,hi) = (1,100)
tag :: Tag
tag = 0

main :: IO ()
main = mpiWorld $ \size rank -> do
   putStrLn $ "Haskell process with rank " ++ show rank ++ " world with size " ++ show size
   if rank == 0
      then do
         (msg :: Msg) <- newListArray bounds [fromIntegral lo .. fromIntegral hi]
         send commWorld 1 tag msg
         _status <- recv commWorld 1 tag msg
         elems <- getElems msg
         putStrLn $ unwords $ map show elems
      else
         putStrLn "This program must be rank 0"
