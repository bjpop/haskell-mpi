{-# LANGUAGE ScopedTypeVariables #-}

module Control.Parallel.MPI.Common
   ( module Datatype
   , module Comm
   , module Status
   , module Tag
   , module Rank
   , module ThreadSupport
   , module Request
   , module Group
   , module Op
   , module ComparisonResult
   , mpi
   , mpiWorld
   , init
   , initThread
   , queryThread
   , isThreadMain
   , finalize
   , commSize
   , commRank
   , commTestInter
   , commRemoteSize
   , commCompare
   , probe
   , barrier
   , wait
   , test
   , cancel
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
   , groupDifference
   , groupCompare
   , groupExcl
   , groupIncl
   , groupTranslateRanks
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
import Control.Parallel.MPI.Utils (checkError, asBool, asInt, asEnum)
import Control.Parallel.MPI.Tag as Tag
import Control.Parallel.MPI.Rank as Rank
import Control.Parallel.MPI.Group as Group
import Control.Parallel.MPI.Op as Op
import Control.Parallel.MPI.ThreadSupport as ThreadSupport
import Control.Parallel.MPI.MarshalUtils (enumToCInt, enumFromCInt)
import Control.Parallel.MPI.ComparisonResult as ComparisonResult
import Control.Concurrent.MVar (MVar, tryTakeMVar, readMVar)
import Control.Concurrent (ThreadId, killThread)

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
initThread required = asEnum $ checkError . Internal.initThread (enumToCInt required)

queryThread :: IO Bool
queryThread = asBool $ checkError . Internal.queryThread
    
isThreadMain :: IO Bool
isThreadMain = asBool $ checkError . Internal.isThreadMain

finalize :: IO ()
finalize = checkError Internal.finalize

commSize :: Comm -> IO Int
commSize comm = asInt $ checkError . Internal.commSize comm

commRank :: Comm -> IO Rank
commRank comm =
   alloca $ \ptr -> do
      checkError $ Internal.commRank comm ptr
      rank <- peek ptr
      return $ toRank rank

commTestInter :: Comm -> IO Bool
commTestInter comm = asBool $ checkError . Internal.commTestInter comm
    
commRemoteSize :: Comm -> IO Int
commRemoteSize comm = asInt $ checkError . Internal.commRemoteSize comm

commCompare :: Comm -> Comm -> IO ComparisonResult
commCompare comm1 comm2 = asEnum $ checkError . Internal.commCompare comm1 comm2

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

groupRank :: Group -> Rank
groupRank = withGroup Internal.groupRank toRank

groupSize :: Group -> Int
groupSize = withGroup Internal.groupSize cIntConv

withGroup :: Storable a => (Group -> Ptr a -> IO CInt) -> (a -> b) -> Group -> b
withGroup prim build group =
   unsafePerformIO $
      alloca $ \ptr -> do
         checkError $ prim group ptr
         build <$> peek ptr

groupUnion :: Group -> Group -> Group
groupUnion g1 g2 = with2Groups Internal.groupUnion id g1 g2

groupIntersection :: Group -> Group -> Group
groupIntersection g1 g2 = with2Groups Internal.groupIntersection id g1 g2

groupDifference :: Group -> Group -> Group
groupDifference g1 g2 = with2Groups Internal.groupDifference id g1 g2

groupCompare :: Group -> Group -> ComparisonResult
groupCompare g1 g2 = with2Groups Internal.groupCompare enumFromCInt g1 g2

with2Groups :: Storable a => (Group -> Group -> Ptr a -> IO CInt) -> (a -> b) -> Group -> Group -> b
with2Groups prim build group1 group2 =
   unsafePerformIO $
      alloca $ \ptr -> do
         checkError $ prim group1 group2 ptr
         build <$> peek ptr

-- Technically it might make better sense to make the second argument a Set rather than a list
-- but the order is significant in the groupIncl function (the other function, not this one).
-- For the sake of keeping their types in sync, a list is used instead.
groupExcl :: Group -> [Rank] -> Group
groupExcl group ranks = groupWithRankList Internal.groupExcl group ranks

groupIncl :: Group -> [Rank] -> Group
groupIncl group ranks = groupWithRankList Internal.groupIncl group ranks

groupWithRankList :: (Group -> CInt -> Ptr CInt -> Ptr Group -> IO CInt) -> Group -> [Rank] -> Group
groupWithRankList prim group ranks =
   unsafePerformIO $ do
      let (rankIntList :: [Int]) = map fromEnum ranks
      alloca $ \groupPtr ->
         withArrayLen rankIntList $ \size ranksPtr -> do
            checkError $ prim group (enumToCInt size) (castPtr ranksPtr) groupPtr
            peek groupPtr

groupTranslateRanks :: Group -> [Rank] -> Group -> [Rank]
groupTranslateRanks group1 ranks group2 =
   unsafePerformIO $ do
      let (rankIntList :: [Int]) = map fromEnum ranks
      withArrayLen rankIntList $ \size ranksPtr ->
         allocaArray size $ \resultPtr -> do
            checkError $ Internal.groupTranslateRanks group1 (enumToCInt size) (castPtr ranksPtr) group2 resultPtr
            map toRank <$> peekArray size resultPtr
