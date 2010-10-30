{-# LANGUAGE ForeignFunctionInterface #-}

{-
status_error
------------
Though Status has `status_error' field, it could be used with care:
"The error field in status is not needed for calls that return only
one status, such as MPI_WAIT, since that would only duplicate the
information returned by the function itself. The current design avoids
the additional overhead of setting it, in such cases. The field is
needed for calls that return multiple statuses, since each request may
have had a different failure." 
(quote from http://mpi-forum.org/docs/mpi22-report/node47.htm#Node47)

This means that, for example, during the call to MPI_Wait
implementation is free to leave this field filled with whatever
garbage got there during memory allocation. Haskell bindings are not
blanking out freshly allocated status for now either. So beware!
-}

#include <mpi.h>

module Control.Parallel.MPI.Status (Status (..), StatusPtr) where

import C2HS
import Control.Monad (liftM)
import Control.Applicative ((<$>), (<*>))

{# context prefix = "MPI" #}

data Status =
   Status
   { status_source :: CInt
   , status_tag :: CInt
   , status_error :: CInt
   , status_count :: CInt
   , status_cancelled :: CInt
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
#ifdef MPICH2
    -- MPICH2 and OpenMPI use different names for the status struct
    -- fields 
    <*> liftM cIntConv ({#get MPI_Status->count #} p)
    <*> liftM cIntConv ({#get MPI_Status->cancelled #} p)
#else
    <*> liftM cIntConv ({#get MPI_Status->_count #} p)
    <*> liftM cIntConv ({#get MPI_Status->_cancelled #} p)
#endif
  poke p x = do
    {#set MPI_Status.MPI_SOURCE #} p (cIntConv $ status_source x)
    {#set MPI_Status.MPI_TAG #} p (cIntConv $ status_tag x)
    {#set MPI_Status.MPI_ERROR #} p (cIntConv $ status_error x)
#ifdef MPICH2
    -- MPICH2 and OpenMPI use different names for the status struct
    -- fields AND different order of fields
    {#set MPI_Status.count #} p (cIntConv $ status_count x)
    {#set MPI_Status.cancelled #} p (cIntConv $ status_cancelled x)
#else
    {#set MPI_Status._count #} p (cIntConv $ status_count x)
    {#set MPI_Status._cancelled #} p (cIntConv $ status_cancelled x)
#endif
