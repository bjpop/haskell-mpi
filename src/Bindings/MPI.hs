module Bindings.MPI 
   ( module Bindings.MPI
   , module Datatype 
   , module Comm
   , module Status
   ) where

import C2HS
import Data.ByteString.Unsafe as BS
import Data.ByteString as BS
import Data.Serialize (encode, decode, Serialize)
import qualified Bindings.MPI.Internal as Internal 
import Bindings.MPI.Datatype as Datatype
import Bindings.MPI.Comm as Comm
import Bindings.MPI.Status as Status 
import Bindings.MPI.MarshalUtils (enumToCInt, enumFromCInt)
import Bindings.MPI.Utils (checkError)

init :: IO ()
init = checkError Internal.init

finalize :: IO ()
finalize = checkError Internal.finalize

commRank :: Enum rank => Comm -> IO rank
commRank comm = 
   alloca $ \ptr -> do
      checkError $ Internal.commRank comm ptr
      rank <- peek ptr
      return $ enumFromCInt rank

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
        
-- XXX should probably bracket here to free malloc'd memory on exceptions.
recvBS :: (Enum source, Enum tag) => source -> tag -> Comm -> IO (Status, ByteString)
recvBS source tag comm = do
  probeStatus <- probe source tag comm
  let count = status_count probeStatus 
  bufferPtr <- mallocBytes count 
  let cSource = enumToCInt source
      cTag    = enumToCInt tag
      cCount  = cIntConv count
  alloca $ \ statusPtr -> do
     checkError $ Internal.recv bufferPtr cCount byte cSource cTag comm (castPtr statusPtr)
     recvStatus <- peek statusPtr
     message <- packCStringLen (castPtr bufferPtr, count)  
     free bufferPtr
     return (recvStatus, message)

probe :: (Enum source, Enum tag) => source -> tag -> Comm -> IO Status
probe source tag comm = do
   let cSource = enumToCInt source
       cTag    = enumToCInt tag
   alloca $ \statusPtr -> do
      checkError $ Internal.probe cSource cTag comm (castPtr statusPtr)
      peek statusPtr
