{- Compile, Run and Clean.

   A helper program for running standalone tests for haskell-mpi.
   Intended to be used in conjunction with shelltestrunner.

   Use like so:

   haskell-mpi-comprunclean -np 2 Pi.hs

   The last argument is the name of a haskell file to compile
   (should be the Main module). All other arguments are given
   to mpirun.

   The program is compiled. The resulting executable is run
   underneath mpirun.

   The executable is deleted and so are temporary files.

   XXX should allow program to be run to accept its own command
       line arguments.
-}

module Main where

import System.Environment (getArgs)
import System.Process (system)
import System.Exit (ExitCode (..), exitWith)
import Control.Monad (when)
import Data.List (isSuffixOf)

main :: IO ()
main = do
   args <- getArgs
   when (length args > 0) $ do
      let mpirunFlags = init args
          (sourceFile, exeFile) = getFileNames $ last args
      run $ "ghc -v0 --make -O2 " ++ sourceFile
      run $ "mpirun " ++ unwords (mpirunFlags ++ [exeFile])
      run $ "rm -f *.o *.hi " ++ exeFile

run :: String -> IO ()
run cmd = do
   -- putStrLn cmd
   status <- system cmd
   if status /= ExitSuccess
      then do
         putStrLn $ "Command failed with status: " ++ show status
         exitWith status
      else return ()

getFileNames :: String -> (String, String)
getFileNames str
   | isSuffixOf ".hs" str = (str, take (length str - 3) str)
   | otherwise = error $ "Not a Haskell filename: " ++ str
