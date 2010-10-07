{-# LANGUAGE ForeignFunctionInterface, GeneralizedNewtypeDeriving #-}

module Control.Parallel.MPI.Rank ( Rank, rankId,
  toRank, fromRank,
  anySource,
  theRoot, procNull ) where

newtype Rank = Rank { rankId :: Int }
   deriving (Eq, Ord, Enum, Num, Integral, Real)

foreign import ccall "mpi_any_source" anySource_ :: Int
foreign import ccall "mpi_root" theRoot_ :: Int
foreign import ccall "mpi_proc_null" procNull_ :: Int

anySource, theRoot, procNull :: Rank
anySource = toRank anySource_
theRoot   = toRank theRoot_
procNull  = toRank procNull_

instance Show Rank where
   show = show . rankId

toRank :: Enum a => a -> Rank
toRank x = Rank { rankId = fromEnum x }

fromRank :: Enum a => Rank -> a
fromRank = toEnum . rankId
