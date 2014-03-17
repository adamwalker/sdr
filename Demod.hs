module Demod where

import Data.Array.Storable
import Foreign.Storable
import Foreign.Ptr
import Foreign.C.Types
import Data.Complex
import Foreign.Marshal.Alloc

foreign import ccall unsafe "fmDemod"
    c_fmDemod :: CInt -> Ptr (Complex CDouble) -> Ptr (Complex CDouble) -> Ptr CDouble -> IO ()

fmDemod :: Int -> StorableArray Int (Complex CDouble) -> IO (StorableArray Int CDouble)
fmDemod samples ina = do
    out <- newArray_ (0, samples - 1)
    withStorableArray ina $ \ip -> 
        withStorableArray out $ \op -> 
        alloca $ \sp -> do
            poke sp (fromIntegral 0)
            c_fmDemod (fromIntegral samples) sp ip op
    return out

