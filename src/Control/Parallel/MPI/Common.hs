module Control.Parallel.MPI.Common
   ( module Datatype
   , module Comm
   , module Status
   , module Tag
   , module Rank
   , mpi
   , init
   , finalize
   , commSize
   , commRank
   , probe
   , barrier
   , wait
   ) where

import Prelude hiding (init)
import C2HS
import qualified Control.Parallel.MPI.Internal as Internal
import Control.Parallel.MPI.Datatype as Datatype
import Control.Parallel.MPI.Comm as Comm
import Control.Parallel.MPI.Request as Request
import Control.Parallel.MPI.Status as Status
import Control.Parallel.MPI.Utils (checkError)
import Control.Parallel.MPI.Tag as Tag
import Control.Parallel.MPI.Rank as Rank

mpi :: IO () -> IO ()
mpi action = init >> action >> finalize

init :: IO ()
init = checkError Internal.init

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
       s <- peek statusPtr
       poke reqPtr request
       checkError $ Internal.wait reqPtr $ castPtr statusPtr
       peek statusPtr
