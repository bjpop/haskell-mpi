module Control.Parallel.MPI.Utils (asBool, asInt, asEnum, debugOut, enumToCInt, enumFromCInt) where

import C2HS

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

enumToCInt :: Enum a => a -> CInt
enumToCInt = cIntConv . fromEnum

enumFromCInt :: Enum a => CInt -> a
enumFromCInt = toEnum . fromIntegral
