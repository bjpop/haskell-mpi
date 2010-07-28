module Control.Parallel.MPI.MarshalUtils 
   ( enumToCInt
   , enumFromCInt
   ) where

import C2HS (cIntConv)
import Foreign.C (CInt)

enumToCInt :: Enum a => a -> CInt
enumToCInt = cIntConv . fromEnum

enumFromCInt :: Enum a => CInt -> a 
enumFromCInt = toEnum . fromIntegral
