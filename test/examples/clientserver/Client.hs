{-# LANGUAGE ScopedTypeVariables #-}

-- Based on the simple client-server example from page 326/327 of 
-- "MPI Standard version 2.2"

module Main where

import System.Environment (getArgs)
import System.Exit
import Foreign.C.Types
import Control.Monad (forM_, when)
import Control.Parallel.MPI.Fast

main :: IO ()
main = do
  args <- getArgs
  when (length args /= 1) $ do
    putStr "server port name required.\n"
    exitWith (ExitFailure 1)
  sendRequest $ head args
  
sendRequest :: String -> IO ()
sendRequest port = mpi $ do
  server <- commConnect port infoNull 0 commWorld
  forM_ [0..4] $ \(i::CInt) -> send server 0 2 i
  send server 0 1 (0xdeadbeef::CInt)
  commDisconnect server