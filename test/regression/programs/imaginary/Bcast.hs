module Main where

import Control.Monad (when)
import Bindings.MPI 

type Msg = (Bool, Int, String, [()])

msg :: Msg 
msg = (True, 12, "fred", [(), (), ()])

root :: Rank
root = toRank 0

main :: IO ()
main = mpi $ do
   newMsg <- bcast msg root commWorld
   rank <- commRank commWorld
   putStrLn $ "rank = " ++ show rank ++ " msg = " ++ show newMsg 
