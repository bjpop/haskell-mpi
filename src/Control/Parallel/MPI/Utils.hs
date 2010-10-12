module Control.Parallel.MPI.Utils (checkError, asBool, asInt, asEnum, debugOut) where

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

asBool :: (Ptr CInt -> IO ()) -> IO Bool
asBool f =
  alloca $ \ptr -> do
    f ptr
    res <- peek ptr
    return $ res /= 0

asInt :: (Ptr CInt -> IO ()) -> IO Int
asInt f =
  alloca $ \ptr -> do
    f ptr
    res <- peek ptr
    return $ cIntConv res

asEnum :: Enum a => (Ptr CInt -> IO ()) -> IO a
asEnum f =
  alloca $ \ptr -> do
    f ptr
    res <- peek ptr
    return $ enumFromCInt res

debugOut :: Show a => a -> Bool
debugOut x = unsafePerformIO $ do
   print x
   return False
