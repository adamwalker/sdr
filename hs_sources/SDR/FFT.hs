{-# LANGUAGE FlexibleContexts, ScopedTypeVariables #-}

{-| Fast FFTs using FFTW -}
module SDR.FFT (
    -- * Windows
    hamming,
    hanning,
    blackman,

    -- * FFTs
    fftw',
    fftw,
    fftwReal',
    fftwReal,
    fftwParallel
    ) where

import           Control.Monad                as CM
import           Foreign.Storable
import           Foreign.Storable.Complex
import           Foreign.C.Types
import           Data.Complex
import           Foreign.ForeignPtr
import           Control.Concurrent           hiding (yield)
import qualified Data.Map                     as Map
import           Data.Coerce

import qualified Data.Vector.Generic          as VG
import qualified Data.Vector.Storable         as VS
import qualified Data.Vector.Storable.Mutable as VSM

import           Pipes
import           Numeric.FFTW

import           SDR.FilterDesign
import           SDR.VectorUtils

mallocForeignBufferAligned :: forall a. Storable a => Int -> IO (ForeignPtr a)
mallocForeignBufferAligned elems = do
    ptr <- fftwMalloc $ fromIntegral $ elems * sizeOf (undefined :: a)
    newForeignPtr fftwFreePtr ptr

-- | Creates a function that performs a complex to complex DFT.
fftw' :: (VG.Vector v (Complex Double)) 
     => Int -- ^ The size of the input and output buffers
     -> IO (v (Complex Double) -> IO (VS.Vector (Complex Double)))
fftw' samples = do
    ina <- mallocForeignBufferAligned samples
    out <- mallocForeignBufferAligned samples

    plan <- withForeignPtr ina $ \ip -> 
        withForeignPtr out $ \op -> 
            planDFT1d samples ip op Forward fftwEstimate
    
    return $ \inv' -> do
        out <- mallocForeignBufferAligned samples
        ina <- mallocForeignBufferAligned samples
        let inv = VSM.unsafeFromForeignPtr0 ina samples

        copyInto inv inv'

        let (fp, offset, length) = VSM.unsafeToForeignPtr inv

        withForeignPtr (coerce fp) $ \fpp -> 
            withForeignPtr (coerce out) $ \op -> 
                executeDFT plan fpp op

        return $ VS.unsafeFromForeignPtr0 out samples

-- | Creates a Pipe that performs a complex to complex DFT.
fftw :: (VG.Vector v (Complex Double)) 
     => Int -- ^ The size of the input and output buffers
     -> IO (Pipe (v (Complex Double)) (VS.Vector (Complex Double)) IO ())
fftw samples = do
    func <- fftw' samples
    return $ for cat $ \dat -> lift (func dat) >>= yield

-- | Creates a function that performs a real to complex DFT.
fftwReal' :: (VG.Vector v Double) 
         => Int -- ^ The size of the input Vector
         -> IO (v Double -> IO (VS.Vector (Complex Double)))
fftwReal' samples = do
    --Allocate in and out buffers that wont be used because there doesnt seem to be a way to create a plan without them
    ina <- mallocForeignBufferAligned samples
    out <- mallocForeignBufferAligned samples

    plan <- withForeignPtr ina $ \ip -> 
        withForeignPtr out $ \op -> 
            planDFTR2C1d samples ip op fftwEstimate

    return $ \inv' -> do
        out <- mallocForeignBufferAligned ((samples `quot` 2) + 1)
        ina <- mallocForeignBufferAligned samples
        let inv = VSM.unsafeFromForeignPtr0 ina samples

        copyInto inv inv'
        let (fp, offset, length) = VSM.unsafeToForeignPtr inv

        withForeignPtr (coerce fp)  $ \fpp -> 
            withForeignPtr (coerce out) $ \op -> 
                executeDFTR2C plan fpp op

        return $ VS.unsafeFromForeignPtr0 out samples

-- | Creates a pipe that performs a real to complex DFT.
fftwReal :: (VG.Vector v Double) 
         => Int -- ^ The size of the input Vector
         -> IO (Pipe (v Double) (VS.Vector (Complex Double)) IO ())
fftwReal samples = do
    func <- fftwReal' samples
    return $ for cat $ \dat -> lift (func dat) >>= yield

{-| Creates a pipe that uses multiple threads to perform complex to complex DFTs in
    a pipelined fashion. Each time a buffer is consumed, it is given to
    a pool of threads to perform the DFT. Then, if a thread has finished
    performing a previous DFT, the result is yielded.
-}
fftwParallel :: (VG.Vector v (Complex Double)) 
             => Int -- ^ The number of threads to use
             -> Int -- ^ The size of the input Vector
             -> IO (Pipe (v (Complex Double)) (VS.Vector (Complex Double)) IO ())
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

        copyInto inv res

        let (fp, offset, length) = VSM.unsafeToForeignPtr inv

        withForeignPtr (coerce fp) $ \fpp -> 
            withForeignPtr (coerce out) $ \op -> 
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

