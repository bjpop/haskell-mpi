{-# LANGUAGE ScopedTypeVariables #-}

-- Based on the simple client-server example from page 326/327 of 
-- "MPI Standard version 2.2"

module Main where

import System.Exit
import Foreign.C.Types
import Control.Monad (forever)
import Control.Parallel.MPI.Fast

main :: IO ()
main = mpi $ do
  size <- commSize commWorld
  if size == 1
    then do
    	port <- openPort infoNull
        putStrLn $ "Server available at port: " ++ show port ++ "."
        forever $ do
          clientComm <- commAccept port infoNull 0 commWorld
          handleRequest port clientComm
    else
    	putStrLn $ "Server too big."

handleRequest :: String -> Comm -> IO ()
handleRequest port client = do
  (msg::CInt, status) <- intoNewVal $ recv client anySource anyTag
  case (status_tag status) of
    0 -> do
      commFree client
      closePort port
      exitWith (ExitFailure 1)
    1 -> commDisconnect client
    2 -> do
      putStrLn $ "Received: " ++ (show msg)
      handleRequest port client
    _ -> abort commWorld 1