module Bindings.MPI where

import qualified Bindings.MPI.Internal as Internal 
import C2HS
import Data.ByteString.Unsafe as BS
import Data.ByteString as BS
import Data.Serialize (encode, decode, Serialize)
import Bindings.MPI.Datatype as Datatype (byte)
import Bindings.MPI.Comm (Comm)
import Bindings.MPI.MarshalUtils (enumToCInt)
import Bindings.MPI.Status (Status (..))

send :: (Serialize msg, Enum dest, Enum tag) => msg -> dest -> tag -> Comm -> IO Int
send = sendBS . encode

sendBS :: (Enum dest, Enum tag) => ByteString -> dest -> tag -> Comm -> IO Int
sendBS bs dest tag comm = do
   let cDest = enumToCInt dest   
       cTag  = enumToCInt tag
       cCount = cIntConv $ BS.length bs
   unsafeUseAsCString bs $ \cString -> do 
       result <- Internal.send (castPtr cString) cCount byte cDest cTag comm
       return $ cIntConv result

recv :: (Serialize msg, Enum source, Enum tag) => source -> tag -> Comm -> IO (Int, Status, msg)
recv source tag comm = do
   (i, status, bs) <- recvBS source tag comm
   case decode bs of
      Left e -> fail e
      Right val -> return (i, status, val)
        
recvBS :: (Enum source, Enum tag) => source -> tag -> Comm -> IO (Int, Status, ByteString)
recvBS source tag comm = do
  (_, probeStatus) <- Internal.probe source tag comm
  let count = status_count probeStatus 
  bufferPtr <- mallocBytes count 
  let cSource = enumToCInt source
      cTag    = enumToCInt tag
      cCount  = cIntConv count
  alloca $ \ statusPtr -> do
     result <- Internal.recv bufferPtr cCount byte cSource cTag comm (castPtr statusPtr)
     recvStatus <- peek statusPtr
     message <- packCStringLen (castPtr bufferPtr, count)  
     free bufferPtr
     return (cIntConv result, recvStatus, message)
