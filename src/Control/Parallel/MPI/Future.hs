module Control.Parallel.MPI.Future where

import Control.Parallel.MPI.Common (Status)
import Control.Concurrent.MVar (MVar, tryTakeMVar, readMVar)
import Control.Concurrent (ThreadId, killThread)

data Future a =
   Future
   { futureThread :: ThreadId
   , futureStatus :: MVar Status
   , futureVal :: MVar a
   }

waitFuture :: Future a -> IO a
waitFuture = readMVar . futureVal

getFutureStatus :: Future a -> IO Status
getFutureStatus = readMVar . futureStatus

pollFuture :: Future a -> IO (Maybe a)
pollFuture = tryTakeMVar . futureVal

-- May want to stop people from waiting on Futures which are killed...
cancelFuture :: Future a -> IO ()
cancelFuture = killThread . futureThread

