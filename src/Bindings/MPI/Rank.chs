{-# LANGUAGE ForeignFunctionInterface, GeneralizedNewtypeDeriving #-}

module Bindings.MPI.Rank (Rank, rankId, toRank, fromRank, anySource) where

newtype Rank = Rank { rankId :: Int }
   deriving (Eq, Ord, Enum, Num)

foreign import ccall "mpi_any_source" anySource_ :: Int 

anySource :: Rank 
anySource = toRank anySource_ 

instance Show Rank where
   show = show . rankId

toRank :: Enum a => a -> Rank
toRank x = Rank { rankId = fromEnum x }

fromRank :: Enum a => Rank -> a
fromRank = toEnum . rankId
