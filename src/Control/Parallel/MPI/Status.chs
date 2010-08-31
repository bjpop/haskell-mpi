{-# LANGUAGE ForeignFunctionInterface #-}

#include <mpi.h>

module Control.Parallel.MPI.Status (Status (..), StatusPtr) where

import C2HS
import Control.Monad (liftM)
import Control.Applicative ((<$>), (<*>))

{# context prefix = "MPI" #}

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
    {#set MPI_Status.MPI_TAG #} p (cIntConv $ status_tag x)
    {#set MPI_Status.MPI_ERROR #} p (cIntConv $ status_error x)
    {#set MPI_Status._count #} p (cIntConv $ status_count x)
    {#set MPI_Status._cancelled #} p (cIntConv $ status_cancelled x)
