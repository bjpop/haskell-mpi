module Control.Parallel.MPI.Utils (asBool, asInt, asEnum, debugOut) where

import Foreign
import Foreign.C.Types
import System.IO.Unsafe as Unsafe

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
    return $ fromIntegral res

asEnum :: Enum a => (Ptr CInt -> IO ()) -> IO a
asEnum f =
  alloca $ \ptr -> do
    f ptr
    res <- peek ptr
    return $ toEnum $ fromIntegral res

debugOut :: Show a => a -> Bool
debugOut x = Unsafe.unsafePerformIO $ do
   print x
   return False
