module Control.Parallel.MPI.Utils (checkError) where

import Foreign.C.Types (CInt)
import Control.Monad (unless)
import Control.Applicative ((<$>))
import Control.Exception.Extensible (throwIO)
import Control.Parallel.MPI.ErrorClasses (ErrorClass (Success))
import Control.Parallel.MPI.MarshalUtils (enumFromCInt)

checkError :: IO CInt -> IO ()
checkError comp = do
   errorClass <- enumFromCInt <$> comp
   unless (errorClass == Success) (throwIO errorClass)
