module Main where

import Control.Monad (when)
import Control.Parallel.MPI.Serializable as Ser
import Control.Parallel.MPI.StorableArray as Stor
import Control.Parallel.MPI.Common
import Data.Array.Storable (StorableArray, newListArray, getElems)

type SerMsg = (Bool, Int, String, [()])
type StorMsg = StorableArray Int Int

serMsg :: SerMsg
serMsg = (True, 12, "fred", [(), (), ()])

range :: (Int, Int)
range@(low, hi) = (1,10)

storMsg :: IO (StorableArray Int Int)
storMsg = newListArray range [low..hi]

root :: Rank
root = toRank 0

main :: IO ()
main = mpi $ do
   newMsg <- Ser.bcast serMsg root commWorld
   rank <- commRank commWorld
   putStrLn $ "Serialized bcast: rank = " ++ show rank ++ " msg = " ++ show newMsg
   arr <- storMsg
   newMsg <- Stor.bcast (arr :: StorMsg) range root commWorld
   elems <- getElems newMsg
   putStrLn $ "StorableArray bcast: rank = " ++ show rank ++ " msg = " ++ show elems
