module Bindings.MPI.MarshalUtils (enumToCInt) where

import C2HS (cIntConv)
import Foreign.C (CInt)

enumToCInt :: Enum a => a -> CInt
enumToCInt = cIntConv . fromEnum
