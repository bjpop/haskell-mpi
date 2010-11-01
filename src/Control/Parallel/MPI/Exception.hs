{-# LANGUAGE DeriveDataTypeable #-}

module Control.Parallel.MPI.Exception (checkError, MPIError (..), ErrorClass (..)) where

import C2HS
import Control.Exception
import Control.Applicative ((<$>))
import Control.Monad (unless)
import Data.Typeable
import Control.Parallel.MPI.Utils (enumFromCInt)
import qualified Control.Parallel.MPI.Internal as Internal (errorClass, errorString, maxErrorString)
import Control.Parallel.MPI.Internal (ErrorClass (..))

data MPIError
   = MPIError
     { mpiErrorClass :: ErrorClass
     , mpiErrorString :: String
     }
   deriving (Eq, Show, Typeable)

instance Exception MPIError

{- From MPI 2.2 report:
 "To make it possible for an application to interpret an error code, the routine
 MPI_ERROR_CLASS converts any error code into one of a small set of standard 
 error codes"
-}

errorClass :: CInt -> IO CInt
errorClass code =
   alloca $ \ptr -> do
      -- We ignore the error code from the call to Internal.errorClass
      -- because we call errorClass from checkError. We'd end up
      -- with an infinite loop if we called checkError here.
      _ <- Internal.errorClass code ptr
      peek ptr

errorString :: CInt -> IO String
errorString code =
  allocaBytes (fromIntegral Internal.maxErrorString) $ \ptr ->
    alloca $ \lenPtr -> do
       -- We ignore the error code from the call to Internal.errorString
       -- because we call errorString from checkError. We'd end up
       -- with an infinite loop if we called checkError here.
       _ <- Internal.errorString code ptr lenPtr
       len <- peek lenPtr
       peekCStringLen (ptr, cIntConv len)

checkError :: IO CInt -> IO ()
checkError comp = do
   code <- comp
   errClass <- enumFromCInt <$> errorClass code
   unless (errClass == Success) $ do
      errStr <- errorString code
      throwIO $ MPIError errClass errStr
