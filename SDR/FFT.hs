{-# LANGUAGE FlexibleContexts #-}
module SDR.FFT where

import Control.Monad
import Data.Bits
import Foreign.Storable
import Foreign.Storable.Complex
import Foreign.C.Types
import Foreign.Ptr
import Data.Complex
import Foreign.Marshal.Utils
import Foreign.Marshal.Array
import Foreign.ForeignPtr

import Data.Vector.Generic as VG
import Data.Vector.Generic.Mutable as VGM
import Data.Vector.Storable as VS
import Data.Vector.Storable.Mutable as VSM
import Data.Vector.Fusion.Stream as VFS

import Pipes
import FFTW

import SDR.Buffer

hanning :: (Floating n, VG.Vector v n) => Int -> v n
hanning size = VG.generate size func
    where
    func idx = 0.5 * (1 - cos((2 * pi * fromIntegral idx) / (fromIntegral size - 1)))

fftw :: (VG.Vector v (Complex CDouble)) => Int -> IO (Pipe (v (Complex CDouble)) (VS.Vector (Complex CDouble)) IO ())
fftw samples = do
    ina <- mallocForeignBufferAligned samples
    out <- mallocForeignBufferAligned samples

    plan <- withForeignPtr ina $ \ip -> 
        withForeignPtr out $ \op -> 
            planDFT1d samples ip op fftwForward fftwEstimate
    
    return $ forever $ do
        out <- lift $ mallocForeignBufferAligned samples
        ina <- lift $ mallocForeignBufferAligned samples
        let inv = VSM.unsafeFromForeignPtr0 ina samples

        inv' <- await
        lift $ VGM.fill inv $ VFS.mapM return $ VG.stream inv'

        let (fp, offset, length) = VSM.unsafeToForeignPtr inv

        lift $ withForeignPtr fp $ \fpp -> 
            withForeignPtr out $ \op -> 
                executeDFT plan fpp op

        yield $ VS.unsafeFromForeignPtr0 out samples

fftwReal :: (VG.Vector v CDouble) => Int -> IO (Pipe (v CDouble) (VS.Vector (Complex CDouble)) IO ())
fftwReal samples = do
    --Allocate in and out buffers that wont be used because there doesnt seem to be a way to create a plan without them
    ina <- mallocForeignBufferAligned samples
    out <- mallocForeignBufferAligned samples

    plan <- withForeignPtr ina $ \ip -> 
        withForeignPtr out $ \op -> 
            planDFTR2C1d samples ip op fftwEstimate

    return $ forever $ do
        out <- lift $ mallocForeignBufferAligned ((samples `quot` 2) + 1)
        ina <- lift $ mallocForeignBufferAligned samples
        let inv = VSM.unsafeFromForeignPtr0 ina samples

        inv' <- await
        lift $ VGM.fill inv $ VFS.mapM return $ VG.stream inv'
        let (fp, offset, length) = VSM.unsafeToForeignPtr inv

        lift $ withForeignPtr fp  $ \fpp -> 
            withForeignPtr out $ \op -> 
                executeDFTR2C plan fpp op

        yield $ VS.unsafeFromForeignPtr0 out samples

