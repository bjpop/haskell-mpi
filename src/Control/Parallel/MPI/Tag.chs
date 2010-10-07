{-# LANGUAGE ForeignFunctionInterface, GeneralizedNewtypeDeriving #-}

module Control.Parallel.MPI.Tag (Tag, toTag, fromTag, tagVal, anyTag) where

newtype Tag = Tag { tagVal :: Int }
   deriving (Eq, Ord, Enum, Num, Integral, Real)

instance Show Tag where
  show = show . tagVal

toTag :: Enum a => a -> Tag
toTag x = Tag { tagVal = fromEnum x }

fromTag :: Enum a => Tag -> a
fromTag = toEnum . tagVal 

foreign import ccall "mpi_any_tag" anyTag_ :: Int 

anyTag :: Tag
anyTag = toTag anyTag_
