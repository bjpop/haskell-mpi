module ExceptionTests (exceptionTests) where

import TestHelpers
import Control.Exception as Ex (try)
import Control.Parallel.MPI.Internal (MPIError(..), ErrorClass(..))
import Control.Parallel.MPI.Serializable

exceptionTests :: Rank -> [(String,TestRunnerTest)]
exceptionTests rank =
  [ mpiTestCase rank "bad rank exception" badRankSend
  ]

-- choose some ridiculously large number for a bad rank
badRank :: Rank
badRank = 10^(9::Int)

-- save and restore the current error handler, but set it
-- to errorsReturn for the nested action.
withErrorsReturn :: IO () -> IO ()
withErrorsReturn action = do
   oldHandler <- commGetErrhandler commWorld
   commSetErrhandler commWorld errorsReturn
   action
   commSetErrhandler commWorld oldHandler

-- All procs try to send a message to a bad rank
badRankSend :: Rank -> IO ()
badRankSend _rank = withErrorsReturn $ do
   result <- try $ send commWorld badRank unitTag "hello"
   errorClass <-
      case result of
         Left e -> return $ mpiErrorClass e
         Right _ -> return $ Success
   errorClass == Rank @? "error class for bad rank send was: " ++ show errorClass ++ ", but expected: Rank"
