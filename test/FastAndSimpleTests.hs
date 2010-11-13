module FastAndSimpleTests (fastAndSimpleTests) where

import TestHelpers
import Control.Parallel.MPI.Fast as Fast
import Control.Parallel.MPI.Simple as Simple

import Data.Serialize ()

fastAndSimpleTests :: Rank -> [(String,TestRunnerTest)]
fastAndSimpleTests rank = 
    [ mpiTestCase rank "mixing Fast and Simple point-to-point operations" sendRecvTest
    ]
    
sendRecvTest :: Rank -> IO ()
sendRecvTest rank
  | rank == sender = do Simple.send commWorld receiver 123 "Sending via Simple"
                        Fast.send commWorld receiver 456 (999.666::Double) -- sending via Fast
  | rank == receiver = do (str, _) <- Simple.recv commWorld sender 123
                          num <- intoNewVal_ $ Fast.recv commWorld sender 456
                          str == "Sending via Simple" @? "Sending via simple failed, got " ++ str
                          num == (999.666 :: Double) @? "Sending via Fast failed, got " ++ show num
  | otherwise = return ()
