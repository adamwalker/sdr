module SDR.Demod where

import Foreign.Storable
import Foreign.Ptr
import Foreign.ForeignPtr
import Foreign.C.Types
import Data.Complex
import Foreign.Marshal.Alloc
import Foreign.Marshal.Array

import Pipes

import SDR.Buffer

foreign import ccall unsafe "fmDemod"
    c_fmDemod :: CInt -> Ptr (Complex CDouble) -> Ptr (Complex CDouble) -> Ptr CDouble -> IO ()

fmDemod :: Int -> Pipe (ForeignPtr (Complex CDouble)) (ForeignPtr CDouble) IO ()
fmDemod samples = fmDemod' 0
    where
    fmDemod' lastVal = do
        ina <- await
        out <- lift $ mallocForeignBufferAligned samples
        last <- lift $ withForeignPtr ina $ \ip -> do
            withForeignPtr out $ \op -> 
                alloca $ \sp -> do
                    poke sp lastVal
                    c_fmDemod (fromIntegral samples) sp ip op
            peek $ advancePtr ip (samples - 1)
        yield out
        fmDemod' last

