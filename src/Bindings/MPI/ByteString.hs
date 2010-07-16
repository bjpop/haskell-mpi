module Bindings.MPI.ByteString where

import qualified Bindings.MPI.Internal as Internal 
import C2HS
import Data.ByteString.Unsafe as BS
import Data.ByteString as BS
import Bindings.MPI.Datatype as Datatype (byte)
import Bindings.MPI.Comm (Comm)
import Bindings.MPI.MarshalUtils (enumToCInt)

-- encode :: Binary a => a -> ByteString

{-
unsafeUseAsCString :: ByteString -> (CString -> IO a) -> IO a

O(1) construction Use a ByteString with a function requiring a CString.

This function does zero copying, and merely unwraps a ByteString to appear as a CString. It is unsafe in two ways:

After calling this function the CString shares the underlying byte buffer with the original ByteString. Thus modifying the CString, either in C, or using poke, will cause the contents of the ByteString to change, breaking referential transparency. Other ByteStrings created by sharing (such as those produced via take or drop) will also reflect these changes. Modifying the CString will break referential transparency. To avoid this, use useAsCString, which makes a copy of the original ByteString.
CStrings are often passed to functions that require them to be null-terminated. If the original ByteString wasn't null terminated, neither will the CString be. It is the programmers responsibility to guarantee that the ByteString is indeed null terminated. If in doubt, use useAsCString.
-}

{-
int MPI_Send( void *buf, int count, MPI_Datatype datatype, int dest, 
              int tag, MPI_Comm comm )
-}

send :: (Enum dest, Enum tag) => ByteString -> dest -> tag -> Comm -> IO Int
send bs dest tag comm = do
   let cDest = enumToCInt dest   
       cTag  = enumToCInt tag
       cCount = cIntConv $ BS.length bs
   unsafeUseAsCString bs $ \cString -> do 
       result <- Internal.send (castPtr cString) cCount byte cDest cTag comm
       return $ cIntConv result
        
-- send_ :: Ptr () -> CInt -> Ptr () -> CInt -> CInt -> Ptr () -> IO CInt


{-
recv :: (Binary msg, Enum source, Enum tag) => source -> tag -> Comm -> IO (Int, Status, msg)
recv source tag comm = do
  let cCount  = cIntConv count
      cSource = enumToCInt source
      cTag    = enumToCInt tag
  alloca $ \ storablePtr ->
     alloca $ \ statusPtr -> do
        result <- recv_ (castPtr storablePtr) cCount dataType cSource cTag comm statusPtr
        status <- peek statusPtr
        message <- peek storablePtr
        return (cIntConv result, status, message)
-}
