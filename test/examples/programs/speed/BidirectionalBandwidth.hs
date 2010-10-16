{-# LANGUAGE ScopedTypeVariables #-}
module Main where

import Control.Parallel.MPI.Common
import Control.Parallel.MPI.Storable
import Data.Array.Storable
import System.Exit

import Foreign.C.Types
import Foreign.Marshal.Array (advancePtr)
import Control.Monad
import Data.IORef
import Text.Printf

benchmark = "OSU MPI Bi-Directional Bandwidth Test"

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
    putStrLn $ printf "%-10s%20s\n" "# Size" "Bi-Bandwidth (MB/s)"

  forM_ (takeWhile (<= max_msg_size) $ iterate (*2) 1) $ \size -> do
    s_buf :: StorableArray Int CChar <- newArray (1,size) 666
    r_buf :: StorableArray Int CChar <- newArray (1,size) 999
    let (loop, skip, window_size) = if (size > large_message_size) 
                                    then (loop_large, skip_large, window_size_large)
                                    else (loop_normal, skip_normal, window_size_normal)
    
    recv_request :: StorableArray Int Request <- newArray_ (1,window_size)
    send_request :: StorableArray Int Request <- newArray_ (1,window_size)
    reqstat      :: StorableArray Int Status  <- newArray_ (1,window_size)

    withStorableArray send_request $ \sendReqPtr ->
      withStorableArray recv_request $ \recvReqPtr -> do
        tref <- newIORef 0
        if myid == 0 then do
          forM_ (takeWhile (< loop+skip) [0..]) $ \i -> do
            when (i == skip) $ do
              t_start <- wtime
              writeIORef tref t_start

            forM_ (takeWhile (<window_size) [0..]) $ \j ->
              irecvPtr commWorld 1 10 (advancePtr recvReqPtr j) r_buf
            forM_ (takeWhile (<window_size) [0..]) $ \j ->
              isendPtr commWorld 1 100 (advancePtr sendReqPtr j) s_buf

            waitall send_request reqstat
            waitall recv_request reqstat

            return ()
          t_end <- wtime
          t_start <- readIORef tref
          let t = t_end - t_start
              total :: Integer = fromIntegral size * fromIntegral loop * fromIntegral window_size * 2
              tmp = (fromIntegral $ total)/1e6;
          putStrLn $ printf ("%-10d%" ++ show field_width ++ "." ++ show float_precision ++ "f") size (tmp / t)
          else do -- myid == 1
          forM_ (takeWhile (< loop+skip) [0..]) $ \i -> do

            forM_ (takeWhile (<window_size) [0..]) $ \j -> do
              irecvPtr commWorld 0 100 (advancePtr recvReqPtr j) r_buf
            forM_ (takeWhile (<window_size) [0..]) $ \j -> do
              isendPtr commWorld 0 10 (advancePtr sendReqPtr j) s_buf

            waitall send_request reqstat
            waitall recv_request reqstat
