{-# LANGUAGE ForeignFunctionInterface, ScopedTypeVariables #-}
module Main where

import Control.Parallel.MPI.Common
import Control.Parallel.MPI.Storable
import Foreign (sizeOf)
import Foreign.C.Types
import Text.Printf
import Control.Monad
import System.Time
import Data.IORef
import Data.Array.Storable

benchmark="OSU MPI All-to-All Personalized Exchange Latency Test"

max_msg_size = 2^20
skip_normal = 300
iterations_normal = 1000
skip_large = 10
iterations_large = 100
max_alignment = 16384
large_message_size = 8192

field_width = 20
float_precision = 2

get_us :: IO Integer
get_us = do
  (TOD sec picosec) <- getClockTime
  return (sec*1000000000000 + picosec)

main = mpi $ do
  rank <- commRank commWorld
  numprocs <- commSize commWorld
  
  let bufferSize = sizeOf ( undefined :: CChar ) * max_msg_size * numprocs + max_alignment
  
  (sendbuf :: StorableArray Int CChar) <- newArray (1,bufferSize) 0
  (recvbuf :: StorableArray Int CChar) <- newArray (1,bufferSize) 0
  
  -- align_size <- getPageSize

  when (rank == 0) $ do
    putStrLn $ printf "# %s" benchmark
    putStrLn $ printf "%-10s%20s\n" "# Size" "Latency (us)"

  barrier commWorld
  forM_ (takeWhile (<= max_msg_size) $ iterate (*2) 1) $ \size -> do
    let (skip, iterations) = if size > large_message_size
                             then (skip_large, iterations_large)
                             else (skip_normal, iterations_normal)
    t1ref <- newIORef 0
    forM_ (takeWhile (< (iterations+skip)) [0..]) $ \i -> do
      when (i == skip) $ do t <- wtime
                            writeIORef t1ref t
      alltoall commWorld sendbuf size recvbuf
    
    when (rank == 0) $ do
      t2 <- wtime
      t1 <- readIORef t1ref
      putStrLn $ printf ("%-10d%" ++ show field_width ++ "." ++ show float_precision ++ "f") size ((t2-t1)/(fromIntegral iterations)*1e6 :: Double)
    
  return ()  