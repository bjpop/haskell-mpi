{-# LANGUAGE ScopedTypeVariables #-}
module Main where

import Control.Parallel.MPI.Simple
import System.Exit

import Foreign.C.Types
import Data.Int
import Control.Monad
import Data.IORef
import Text.Printf
import Data.Array.Storable

import qualified Data.ByteString.Char8 as BS

benchmark = "OSU MPI Bandwidth Test (Serializable)"

max_req_num = 1000

max_alignment = 65536
max_msg_size = 2^22
mybufsize = (max_msg_size + max_alignment)

loop_normal = 100
window_size_normal = 64
skip_normal = 10

loop_large = 20
window_size_large = 64
skip_large = 2

large_message_size = 8192

field_width = 20
float_precision = 2

main = mpi $ do

  myid <- commRank commWorld
  numprocs <- commSize commWorld

  when (numprocs /= 2) $ do
    when (myid == 0) $ do
      putStrLn "This test requires exactly two processes"
    exitWith (ExitFailure 1)

  when (myid == 0) $ do
    putStrLn $ printf "# %s" benchmark
    putStrLn $ printf "%-10s%20s\n" "# Size" "Bandwidth (MB/s)"

  forM_ (takeWhile (<= max_msg_size) $ iterate (*2) 1) $ \size -> do
    let s_buf :: BS.ByteString = BS.replicate size 's'
    
    let (loop, skip, window_size) = if (size > large_message_size) 
                                    then (loop_large, skip_large, window_size_large)
                                    else (loop_normal, skip_normal, window_size_normal)
    
    tref <- newIORef 0
    if myid == 0 then do
      forM_ (takeWhile (< loop+skip) [0..]) $ \i -> do
        when (i == skip) $ do
          t_start <- wtime
          writeIORef tref t_start

        requests <- forM (takeWhile (<window_size) [0..]) $ \j ->
          isend commWorld 1 100 s_buf

        waitall requests

        (deadbeef::Int, _) <- recv commWorld 1 101
        return ()

      t_end <- wtime
      t_start <- readIORef tref
      let t = t_end - t_start
          total :: Integer = fromIntegral size * fromIntegral loop * fromIntegral window_size
          tmp = (fromIntegral $ total)/1e6;
      putStrLn $ printf ("%-10d%" ++ show field_width ++ "." ++ show float_precision ++ "f") size (tmp / t)
      else do -- myid == 1
      forM_ (takeWhile (< loop+skip) [0..]) $ \i -> do

        futures :: [Future BS.ByteString] <- forM (takeWhile (<window_size) [0..]) $ \j -> do
          recvFuture commWorld 0 100 

        mapM_ waitFuture futures
        send commWorld 0 101 (0xdeadbeef::Int)
