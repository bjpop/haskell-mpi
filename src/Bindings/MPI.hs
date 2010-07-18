module Bindings.MPI 
   ( module Bindings.MPI
   , module Datatype 
   , module Comm
   , module Status
   ) where

import C2HS
import Control.Exception.Extensible (bracket)
import Data.ByteString.Unsafe as BS
import Data.ByteString as BS
import Data.Serialize (encode, decode, Serialize)
import qualified Bindings.MPI.Internal as Internal 
import Bindings.MPI.Datatype as Datatype
import Bindings.MPI.Comm as Comm
import Bindings.MPI.Request as Request
import Bindings.MPI.Status as Status 
import Bindings.MPI.MarshalUtils (enumToCInt, enumFromCInt)
import Bindings.MPI.Utils (checkError)

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

commRank :: Enum rank => Comm -> IO rank
commRank comm = 
   alloca $ \ptr -> do
      checkError $ Internal.commRank comm ptr
      rank <- peek ptr
      return $ enumFromCInt rank

probe :: (Enum source, Enum tag) => source -> tag -> Comm -> IO Status
probe source tag comm = do
   let cSource = enumToCInt source
       cTag    = enumToCInt tag
   alloca $ \statusPtr -> do
      checkError $ Internal.probe cSource cTag comm (castPtr statusPtr)
      peek statusPtr

send :: (Serialize msg, Enum dest, Enum tag) => msg -> dest -> tag -> Comm -> IO () 
send = sendBS . encode

sendBS :: (Enum dest, Enum tag) => ByteString -> dest -> tag -> Comm -> IO () 
sendBS bs dest tag comm = do
   let cDest = enumToCInt dest   
       cTag  = enumToCInt tag
       cCount = cIntConv $ BS.length bs
   unsafeUseAsCString bs $ \cString -> 
       checkError $ Internal.send (castPtr cString) cCount byte cDest cTag comm

recv :: (Serialize msg, Enum source, Enum tag) => source -> tag -> Comm -> IO (Status, msg)
recv source tag comm = do
   (status, bs) <- recvBS source tag comm
   case decode bs of
      Left e -> fail e
      Right val -> return (status, val)
        
recvBS :: (Enum source, Enum tag) => source -> tag -> Comm -> IO (Status, ByteString)
recvBS source tag comm = do
   probeStatus <- probe source tag comm
   let count = status_count probeStatus 
   let cSource = enumToCInt source
       cTag    = enumToCInt tag
       cCount  = cIntConv count
   bracket 
      (mallocBytes count)
      free 
      (\bufferPtr -> 
          alloca $ \statusPtr -> do
             checkError $ Internal.recv bufferPtr cCount byte cSource cTag comm $ castPtr statusPtr
             recvStatus <- peek statusPtr
             message <- packCStringLen (castPtr bufferPtr, count)  
             return (recvStatus, message))

iSend :: (Serialize msg, Enum dest, Enum tag) => msg -> dest -> tag -> Comm -> IO Request 
iSend = iSendBS . encode

iSendBS :: (Enum dest, Enum tag) => ByteString -> dest -> tag -> Comm -> IO Request 
iSendBS bs dest tag comm = do
   let cDest = enumToCInt dest   
       cTag  = enumToCInt tag
       cCount = cIntConv $ BS.length bs
   alloca $ \requestPtr -> 
      unsafeUseAsCString bs $ \cString -> do
          checkError $ Internal.iSend (castPtr cString) cCount byte cDest cTag comm requestPtr
          peek requestPtr 

iRecv :: (Serialize msg, Enum source, Enum tag) => source -> tag -> Comm -> IO (Request, msg)
iRecv source tag comm = do
   (request, bs) <- iRecvBS source tag comm
   case decode bs of
      Left e -> fail e
      Right val -> return (request, val)
        
iRecvBS :: (Enum source, Enum tag) => source -> tag -> Comm -> IO (Request, ByteString)
iRecvBS source tag comm = do
   probeStatus <- probe source tag comm
   let count = status_count probeStatus
   let cSource = enumToCInt source
       cTag    = enumToCInt tag
       cCount  = cIntConv count
   bracket 
      (mallocBytes count)
      free
      (\bufferPtr -> do 
           alloca $ \requestPtr -> do
              checkError $ Internal.iRecv bufferPtr cCount byte cSource cTag comm $ castPtr requestPtr
              request <- peek requestPtr 
              message <- packCStringLen (castPtr bufferPtr, count)  
              return (request, message))
{-
   bufferPtr <- mallocBytes count
   alloca $ \requestPtr  -> do
      checkError $ Internal.iRecv bufferPtr cCount byte cSource cTag comm (castPtr requestPtr)
      request <- peek requestPtr 
      message <- packCStringLen (castPtr bufferPtr, count)  
      free bufferPtr
      return (request, message)
-}
