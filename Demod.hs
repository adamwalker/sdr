module Demod where

import Foreign.Storable
import Foreign.Ptr
import Foreign.ForeignPtr
import Foreign.C.Types
import Data.Complex
import Foreign.Marshal.Alloc

foreign import ccall unsafe "fmDemod"
    c_fmDemod :: CInt -> Ptr (Complex CDouble) -> Ptr (Complex CDouble) -> Ptr CDouble -> IO ()

fmDemod :: Int -> ForeignPtr (Complex CDouble) -> IO (ForeignPtr CDouble)
fmDemod samples ina = do
    out <- mallocForeignPtrArray samples
    withForeignPtr ina $ \ip -> 
        withForeignPtr out $ \op -> 
        alloca $ \sp -> do
            poke sp (fromIntegral 0)
            c_fmDemod (fromIntegral samples) sp ip op
    return out

