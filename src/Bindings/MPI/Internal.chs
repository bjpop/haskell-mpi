{-# LANGUAGE ForeignFunctionInterface #-}

#include <mpi.h>
#include "init_wrapper.h"

module Bindings.MPI.Internal 
   (Comm, Datatype, Status (..), init, finalize, send, recv, commWorld, commRank, int) where

import Prelude hiding (init, error)
import C2HS
import Control.Monad (liftM)
import Control.Applicative ((<$>), (<*>))

{# context prefix = "MPI" #}

type Comm = {# type MPI_Comm #}
type Datatype = {# type MPI_Datatype #}

data Status = 
   Status 
   { status_source :: Int
   , status_tag :: Int
   , status_error :: Int
   , status_count :: Int
   , status_cancelled :: Int 
   }
   deriving (Eq, Ord, Show)

{#pointer *Status as StatusPtr -> Status #}

instance Storable Status where
  sizeOf _ = {#sizeof MPI_Status #}
  alignment _ = 4
  peek p = Status 
    <$> liftM cIntConv ({#get MPI_Status->MPI_SOURCE #} p)
    <*> liftM cIntConv ({#get MPI_Status->MPI_TAG #} p)
    <*> liftM cIntConv ({#get MPI_Status->MPI_ERROR #} p)
    <*> liftM cIntConv ({#get MPI_Status->_count #} p)
    <*> liftM cIntConv ({#get MPI_Status->_cancelled #} p)
  poke p x = do
    {#set MPI_Status.MPI_SOURCE #} p (cIntConv $ status_source x)
    {#set MPI_Status.MPI_SOURCE #} p (cIntConv $ status_tag x)
    {#set MPI_Status.MPI_ERROR #} p (cIntConv $ status_error x)
    {#set MPI_Status._count #} p (cIntConv $ status_count x)
    {#set MPI_Status._cancelled #} p (cIntConv $ status_cancelled x)

foreign import ccall "mpi_comm_world" commWorld :: Comm
foreign import ccall "mpi_int" int :: Datatype

{# fun unsafe init_wrapper as init {} -> `Int' #}

{# fun unsafe Finalize as ^ {} -> `Int' #}

withStorable :: Storable a => a -> (Ptr a -> IO b) -> IO b
withStorable x f = alloca (\ptr -> poke ptr x >> f ptr)

withStorableCast :: Storable a => a -> (Ptr c -> IO b) -> IO b
withStorableCast x f = withStorable x (f . castPtr)

-- int MPI_Comm_rank(MPI_Comm comm, int *rank)
{# fun unsafe Comm_rank as ^ { id `Comm', alloca- `Int' peekIntConv* } -> `Int' #}

-- int MPI_Send(void *buf, int count, MPI_Datatype datatype, int dest, int tag, MPI_Comm comm)
{# fun unsafe Send as ^ `Storable a' => { withStorableCast* `a', `Int', id `Datatype', `Int', `Int', id `Comm' } -> `Int' #}

-- int MPI_Recv(void *buf, int count, MPI_Datatype datatype, int source, int tag, MPI_Comm comm, MPI_Status *status)
-- recv = {# call unsafe MPI_Recv as ^ #}
{# fun unsafe Recv as ^ `Storable a' => { withStorableCast* `a', `Int', id `Datatype', `Int', `Int', id `Comm', alloca- `Status' peek* } -> `Int' #}
