{-# LANGUAGE ForeignFunctionInterface, GeneralizedNewtypeDeriving #-}
-----------------------------------------------------------------------------
{- |
Module      : Control.Parallel.MPI.Tag
Copyright   : (c) 2010 Bernie Pope, Dmitry Astapov
License     : BSD-style
Maintainer  : florbitous@gmail.com
Stability   : experimental
Portability : ghc

This module provides Haskell datatype that represents values which
could be used as MPI tags.
-}
-----------------------------------------------------------------------------
module Control.Parallel.MPI.Tag (Tag, toTag, fromTag, tagVal, anyTag) where

import Foreign

-- TODO: actually, this is a 32-bit int, and even less than that.
-- See section 8 of MPI report about extracting MPI_TAG_UB
-- and using it here
newtype Tag = Tag { tagVal :: Int }
   deriving (Eq, Ord, Enum, Num, Integral, Real)

instance Show Tag where
  show = show . tagVal

toTag :: Enum a => a -> Tag
toTag x = Tag { tagVal = fromEnum x }

fromTag :: Enum a => Tag -> a
fromTag = toEnum . tagVal 

foreign import ccall unsafe "&mpi_any_tag" anyTag_ :: Ptr Int 

-- | Predefined tag value that allows reception of the messages with
--   arbitrary tag values. Corresponds to @MPI_ANY_TAG@.
anyTag :: Tag
anyTag = toTag $ unsafePerformIO $ peek anyTag_
