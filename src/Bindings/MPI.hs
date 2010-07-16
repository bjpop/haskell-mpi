module Bindings.MPI 
   ( module Bindings.MPI
   , module Internal
   , module Datatype 
   , module Comm
   , module Status
   ) where

import C2HS
import Data.ByteString.Unsafe as BS
import Data.ByteString as BS
import Data.Serialize (encode, decode, Serialize)
import Bindings.MPI.Internal as Internal hiding (send, recv, commRank) 
import qualified Bindings.MPI.Internal as BMI (send, recv, commRank)
import Bindings.MPI.Datatype as Datatype
import Bindings.MPI.Comm as Comm
import Bindings.MPI.Status as Status 
import Bindings.MPI.MarshalUtils (enumToCInt, enumFromCInt)

commRank :: Enum rank => Comm -> IO (Int, rank)
commRank comm = 
   alloca $ \ptr -> do
      result <- BMI.commRank comm ptr 
      rank <- peek ptr
      return (cIntConv result, enumFromCInt rank)

send :: (Serialize msg, Enum dest, Enum tag) => msg -> dest -> tag -> Comm -> IO Int
send = sendBS . encode

sendBS :: (Enum dest, Enum tag) => ByteString -> dest -> tag -> Comm -> IO Int
sendBS bs dest tag comm = do
   let cDest = enumToCInt dest   
       cTag  = enumToCInt tag
       cCount = cIntConv $ BS.length bs
   unsafeUseAsCString bs $ \cString -> do 
       result <- BMI.send (castPtr cString) cCount byte cDest cTag comm
       return $ cIntConv result

recv :: (Serialize msg, Enum source, Enum tag) => source -> tag -> Comm -> IO (Int, Status, msg)
recv source tag comm = do
   (i, status, bs) <- recvBS source tag comm
   case decode bs of
      Left e -> fail e
      Right val -> return (i, status, val)
        
-- XXX should probably bracket here to free malloc'd memory on exceptions.
recvBS :: (Enum source, Enum tag) => source -> tag -> Comm -> IO (Int, Status, ByteString)
recvBS source tag comm = do
  (_, probeStatus) <- Internal.probe source tag comm
  let count = status_count probeStatus 
  bufferPtr <- mallocBytes count 
  let cSource = enumToCInt source
      cTag    = enumToCInt tag
      cCount  = cIntConv count
  alloca $ \ statusPtr -> do
     result <- BMI.recv bufferPtr cCount byte cSource cTag comm (castPtr statusPtr)
     recvStatus <- peek statusPtr
     message <- packCStringLen (castPtr bufferPtr, count)  
     free bufferPtr
     return (cIntConv result, recvStatus, message)
