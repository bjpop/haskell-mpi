{-# LANGUAGE ForeignFunctionInterface, GeneralizedNewtypeDeriving #-}

module Control.Parallel.MPI.Tag (Tag, toTag, fromTag, tagVal, anyTag) where

import Foreign

newtype Tag = Tag { tagVal :: Int }
   deriving (Eq, Ord, Enum, Num, Integral, Real)

instance Show Tag where
  show = show . tagVal

toTag :: Enum a => a -> Tag
toTag x = Tag { tagVal = fromEnum x }

fromTag :: Enum a => Tag -> a
fromTag = toEnum . tagVal 

foreign import ccall unsafe "&mpi_any_tag" anyTag_ :: Ptr Int 

anyTag :: Tag
anyTag = toTag $ unsafePerformIO $ peek anyTag_
