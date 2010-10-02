module Control.Parallel.MPI.Utils (checkError, intoBool, intoInt, intoEnum) where

import C2HS
import Control.Monad (unless)
import Control.Applicative ((<$>))
import Control.Exception.Extensible (throwIO)
import Control.Parallel.MPI.ErrorClasses (ErrorClass (Success))
import Control.Parallel.MPI.MarshalUtils (enumFromCInt)

checkError :: IO CInt -> IO ()
checkError comp = do
   errorClass <- enumFromCInt <$> comp
   unless (errorClass == Success) (throwIO errorClass)

intoBool :: (Ptr CInt -> IO ()) -> IO Bool
intoBool f =
  alloca $ \ptr -> do
    f ptr
    res <- peek ptr
    return $ res /= 0

intoInt :: (Ptr CInt -> IO ()) -> IO Int
intoInt f =
  alloca $ \ptr -> do
    f ptr
    res <- peek ptr
    return $ cIntConv res
    
intoEnum :: Enum a => (Ptr CInt -> IO ()) -> IO a
intoEnum f =
  alloca $ \ptr -> do
    f ptr
    res <- peek ptr
    return $ enumFromCInt res
