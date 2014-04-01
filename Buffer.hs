{-# LANGUAGE ScopedTypeVariables #-}
module Buffer where

import Foreign.Storable
import Foreign.ForeignPtr

import FFTW

mallocForeignBufferAligned :: forall a. Storable a => Int -> IO (ForeignPtr a)
mallocForeignBufferAligned elems = do
    ptr <- fftwMalloc $ fromIntegral $ elems * sizeOf (undefined :: a)
    newForeignPtr fftwFreePtr ptr
