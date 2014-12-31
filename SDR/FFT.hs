{-# LANGUAGE FlexibleContexts, ScopedTypeVariables #-}
module SDR.FFT (
    hanning,
    fftFixup,
    fftw,
    fftwParallel,
    fftwReal
    ) where

import Control.Monad as CM
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

import Control.Concurrent hiding (yield)
import Control.Concurrent.Chan
import Control.Concurrent.MVar

import Data.Map (Map)
import qualified Data.Map as Map

import Pipes
import FFTW

mallocForeignBufferAligned :: forall a. Storable a => Int -> IO (ForeignPtr a)
mallocForeignBufferAligned elems = do
    ptr <- fftwMalloc $ fromIntegral $ elems * sizeOf (undefined :: a)
    newForeignPtr fftwFreePtr ptr

hanning :: (Floating n, VG.Vector v n) => Int -> v n
hanning size = VG.generate size func
    where
    func idx = 0.5 * (1 - cos((2 * pi * fromIntegral idx) / (fromIntegral size - 1)))

fftFixup :: (VG.Vector v n, Num n) => Int -> v n
fftFixup size = VG.generate size func
    where
    func idx 
        | even idx = 1
        | odd  idx = -1

fftw :: (VG.Vector v (Complex CDouble)) => Int -> IO (Pipe (v (Complex CDouble)) (VS.Vector (Complex CDouble)) IO ())
fftw samples = do
    ina <- mallocForeignBufferAligned samples
    out <- mallocForeignBufferAligned samples

    plan <- withForeignPtr ina $ \ip -> 
        withForeignPtr out $ \op -> 
            planDFT1d samples ip op Forward fftwEstimate
    
    return $ for cat $ \inv' -> do
        out <- lift $ mallocForeignBufferAligned samples
        ina <- lift $ mallocForeignBufferAligned samples
        let inv = VSM.unsafeFromForeignPtr0 ina samples

        lift $ VGM.fill inv $ VFS.mapM return $ VG.stream inv'

        let (fp, offset, length) = VSM.unsafeToForeignPtr inv

        lift $ withForeignPtr fp $ \fpp -> 
            withForeignPtr out $ \op -> 
                executeDFT plan fpp op

        yield $ VS.unsafeFromForeignPtr0 out samples

fftwParallel :: (VG.Vector v (Complex CDouble)) => Int -> Int -> IO (Pipe (v (Complex CDouble)) (VS.Vector (Complex CDouble)) IO ())
fftwParallel threads samples = do
    --plan the DFT
    ina <- mallocForeignBufferAligned samples
    out <- mallocForeignBufferAligned samples

    plan <- withForeignPtr ina $ \ip -> 
        withForeignPtr out $ \op -> 
            planDFT1d samples ip op Forward fftwEstimate

    --setup the channels and worker threads
    inChan <- newChan 
    outMap <- newMVar Map.empty

    CM.replicateM threads $ forkIO $ forever $ do
        (idx, res) <- readChan inChan
    
        out <- mallocForeignBufferAligned samples
        ina <- mallocForeignBufferAligned samples
        let inv = VSM.unsafeFromForeignPtr0 ina samples

        VGM.fill inv $ VFS.mapM return $ VG.stream res

        let (fp, offset, length) = VSM.unsafeToForeignPtr inv

        withForeignPtr fp $ \fpp -> 
            withForeignPtr out $ \op -> 
                executeDFT plan fpp op

        theMap <- takeMVar outMap
        putMVar outMap $ Map.insert idx (VS.unsafeFromForeignPtr0 out samples) theMap
   
    --build the pipe
    let pipe nextIn nextOut = do
            dat <- await
            lift $ writeChan inChan (nextIn, dat)

            theMap <- lift $ takeMVar outMap
            case Map.lookup nextOut theMap of
                Nothing  -> do
                    lift $ putMVar outMap theMap
                    pipe (nextIn + 1) nextOut
                Just dat -> do
                    lift $ putMVar outMap $ Map.delete nextOut theMap
                    yield dat
                    pipe (nextIn + 1) (nextOut + 1)

    return $ pipe 0 0

fftwReal :: (VG.Vector v CDouble) => Int -> IO (Pipe (v CDouble) (VS.Vector (Complex CDouble)) IO ())
fftwReal samples = do
    --Allocate in and out buffers that wont be used because there doesnt seem to be a way to create a plan without them
    ina <- mallocForeignBufferAligned samples
    out <- mallocForeignBufferAligned samples

    plan <- withForeignPtr ina $ \ip -> 
        withForeignPtr out $ \op -> 
            planDFTR2C1d samples ip op fftwEstimate

    return $ for cat $ \inv' -> do
        out <- lift $ mallocForeignBufferAligned ((samples `quot` 2) + 1)
        ina <- lift $ mallocForeignBufferAligned samples
        let inv = VSM.unsafeFromForeignPtr0 ina samples

        lift $ VGM.fill inv $ VFS.mapM return $ VG.stream inv'
        let (fp, offset, length) = VSM.unsafeToForeignPtr inv

        lift $ withForeignPtr fp  $ \fpp -> 
            withForeignPtr out $ \op -> 
                executeDFTR2C plan fpp op

        yield $ VS.unsafeFromForeignPtr0 out samples

