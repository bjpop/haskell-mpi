module Control.Parallel.MPI.Common
   ( module Datatype
   , module Comm
   , module Status
   , module Tag
   , module Rank
   , module ThreadSupport
   , module Request
   , module Group
   , mpi
   , mpiWorld
   , init
   , initThread
   , finalize
   , commSize
   , commRank
   , probe
   , barrier
   , wait
   , test
   , cancel
   , zeroRank
   , unitTag
   , Future(..)
   , waitFuture
   , getFutureStatus
   , pollFuture
   , cancelFuture
   , wtime
   , wtick
   , commGroup
   , groupRank
   , groupSize
   , groupUnion
   , groupIntersection
   ) where

import Prelude hiding (init)
import C2HS
import Control.Applicative ((<$>))
import Control.Exception (finally)
import qualified Control.Parallel.MPI.Internal as Internal
import Control.Parallel.MPI.Datatype as Datatype
import Control.Parallel.MPI.Comm as Comm
import Control.Parallel.MPI.Request as Request
import Control.Parallel.MPI.Status as Status
import Control.Parallel.MPI.Utils (checkError)
import Control.Parallel.MPI.Tag as Tag
import Control.Parallel.MPI.Rank as Rank
import Control.Parallel.MPI.Group as Group
import Control.Parallel.MPI.ThreadSupport as ThreadSupport
import Control.Parallel.MPI.MarshalUtils (enumToCInt, enumFromCInt)
import Control.Concurrent.MVar (MVar, tryTakeMVar, readMVar)
import Control.Concurrent (ThreadId, killThread)

zeroRank :: Rank
zeroRank = toRank (0::Int)

unitTag :: Tag
unitTag = toTag ()

mpi :: IO () -> IO ()
mpi action = init >> (action `finally` finalize)

mpiWorld :: (Int -> Rank -> IO ()) -> IO ()
mpiWorld action = do
   init
   size <- commSize commWorld
   rank <- commRank commWorld
   action size rank `finally` finalize

init :: IO ()
init = checkError Internal.init

initThread :: ThreadSupport -> IO ThreadSupport
initThread required = 
  alloca $ \providedPtr -> do
    checkError (Internal.initThread (enumToCInt required) (castPtr providedPtr))
    provided <- peek providedPtr
    return (enumFromCInt provided)

finalize :: IO ()
finalize = checkError Internal.finalize

commSize :: Comm -> IO Int
commSize comm = do
   alloca $ \ptr -> do
      checkError $ Internal.commSize comm ptr
      size <- peek ptr
      return $ cIntConv size

commRank :: Comm -> IO Rank
commRank comm =
   alloca $ \ptr -> do
      checkError $ Internal.commRank comm ptr
      rank <- peek ptr
      return $ toRank rank

probe :: Rank -> Tag -> Comm -> IO Status
probe rank tag comm = do
   let cSource = fromRank rank
       cTag    = fromTag tag
   alloca $ \statusPtr -> do
      checkError $ Internal.probe cSource cTag comm $ castPtr statusPtr
      peek statusPtr

barrier :: Comm -> IO ()
barrier comm = checkError $ Internal.barrier comm

wait :: Request -> IO Status
wait request =
   alloca $ \statusPtr ->
     alloca $ \reqPtr -> do
       poke reqPtr request
       checkError $ Internal.wait reqPtr $ castPtr statusPtr
       peek statusPtr

-- Returns Nothing if the request is not complete, otherwise
-- it returns (Just status).
test :: Request -> IO (Maybe Status)
test request =
    alloca $ \statusPtr ->
       alloca $ \reqPtr ->
          alloca $ \flagPtr -> do
              poke reqPtr request
              checkError $ Internal.test reqPtr (castPtr flagPtr) (castPtr statusPtr)
              flag <- peek flagPtr
              if flag
                 then Just <$> peek statusPtr
                 else return Nothing

cancel :: Request -> IO ()
cancel request =
   alloca $ \reqPtr -> do
       poke reqPtr request
       checkError $ Internal.cancel reqPtr

wtime, wtick :: IO Double
wtime = do
   res <- Internal.wtime
   return $ realToFrac res

wtick = do
   res <- Internal.wtick
   return $ realToFrac res


-- Futures
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

commGroup :: Comm -> IO Group
commGroup comm =
   alloca $ \ptr -> do
      checkError $ Internal.commGroup comm ptr
      peek ptr

groupRank :: Group -> IO Rank
groupRank = withGroup Internal.groupRank toRank

groupSize :: Group -> IO Int
groupSize = withGroup Internal.groupSize cIntConv

withGroup :: Storable a => (Group -> Ptr a -> IO CInt) -> (a -> b) -> Group -> IO b
withGroup prim build group =
   alloca $ \ptr -> do
      checkError $ prim group ptr
      r <- peek ptr
      return $ build r

-- XXX does this need an IO type?
groupUnion :: Group -> Group -> IO Group
groupUnion = with2Groups Internal.groupUnion id
{-
groupUnion g1 g2 =
   alloca $ \ptr -> do
      checkError $ Internal.groupUnion g1 g2 ptr
      peek ptr
-}

-- XXX does this need an IO type?
groupIntersection :: Group -> Group -> IO Group
groupIntersection = with2Groups Internal.groupIntersection id

with2Groups :: Storable a => (Group -> Group -> Ptr a -> IO CInt) -> (a -> b) -> Group -> Group -> IO b
with2Groups prim build group1 group2 =
   alloca $ \ptr -> do
      checkError $ prim group1 group2 ptr
      r <- peek ptr
      return $ build r
