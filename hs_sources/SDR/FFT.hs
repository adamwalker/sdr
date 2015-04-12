{-# LANGUAGE FlexibleContexts, ScopedTypeVariables #-}

{-| Fast FFTs using FFTW -}
module SDR.FFT (
    -- * Windows
    hanning,
    fftFixup,

    -- * FFTs
    fftw,
    fftwReal,
    fftwParallel
    ) where

import Control.Monad as CM
import Foreign.Storable
import Foreign.Storable.Complex
import Foreign.C.Types
import Data.Complex
import Foreign.ForeignPtr

import Data.Vector.Generic as VG
import Data.Vector.Generic.Mutable as VGM
import Data.Vector.Storable as VS
import Data.Vector.Storable.Mutable as VSM
import Data.Vector.Fusion.Stream as VFS

import Control.Concurrent hiding (yield)

import qualified Data.Map as Map

import Pipes
import Numeric.FFTW

mallocForeignBufferAligned :: forall a. Storable a => Int -> IO (ForeignPtr a)
mallocForeignBufferAligned elems = do
    ptr <- fftwMalloc $ fromIntegral $ elems * sizeOf (undefined :: a)
    newForeignPtr fftwFreePtr ptr

-- | Compute a Hanning window. You probably want to multiply your buffers by one of these before you perform a DFT.
hanning :: (Floating n, VG.Vector v n) 
        => Int -- ^ The length of the window
        -> v n
hanning size = VG.generate size func
    where
    func idx = 0.5 * (1 - cos((2 * pi * fromIntegral idx) / (fromIntegral size - 1)))

-- | Compute a vector of alternating 1s and 0s of the given size.
fftFixup :: (VG.Vector v n, Num n) 
         => Int -- ^ The length of the Vector
         -> v n
fftFixup size = VG.generate size func
    where
    func idx 
        | even idx  = 1
        | otherwise = -1

-- | Creates a Pipe that performs a complex to complex DFT.
fftw :: (VG.Vector v (Complex CDouble)) 
     => Int -- ^ The size of the input and output buffers
     -> IO (Pipe (v (Complex CDouble)) (VS.Vector (Complex CDouble)) IO ())
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

-- | Creates a pipe that performs a real to complex DFT.
fftwReal :: (VG.Vector v CDouble) 
         => Int -- ^ The size of the input Vector
         -> IO (Pipe (v CDouble) (VS.Vector (Complex CDouble)) IO ())
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

{-| Creates a pipe that uses multiple threads to perform complex to complex DFTs in
    a pipelined fashion. Each time a buffer is consumed, it is given to
    a pool of threads to perform the DFT. Then, if a thread has finished
    performing a previous DFT, the result is yielded.
-}
fftwParallel :: (VG.Vector v (Complex CDouble)) 
             => Int -- ^ The number of threads to use
             -> Int -- ^ The size of the input Vector
             -> IO (Pipe (v (Complex CDouble)) (VS.Vector (Complex CDouble)) IO ())
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

