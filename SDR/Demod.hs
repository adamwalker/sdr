{-# LANGUAGE FlexibleContexts #-}
module SDR.Demod where

import Foreign.Storable
import Foreign.Ptr
import Foreign.ForeignPtr
import Foreign.C.Types
import Data.Complex
import Foreign.Marshal.Alloc
import Foreign.Marshal.Array
import Data.Vector.Generic
import Data.Vector.Fusion.Stream

import Pipes

import SDR.Buffer
import SDR.Util

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

fmDemodStr :: (RealFloat a) => Complex a -> Stream (Complex a) -> Stream a
fmDemodStr = mapAccumMV func 
    where
    func last sample = return (sample, phase (sample * conjugate last))

fmDemodVec :: (RealFloat a, Vector v (Complex a), Vector v a) => Complex a -> v (Complex a) -> v a
fmDemodVec init = unstream . fmDemodStr init . stream

