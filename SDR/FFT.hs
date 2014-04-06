module SDR.FFT where

import Control.Monad
import Data.Bits
import Foreign.Storable
import Foreign.Storable.Complex
import Foreign.C.Types
import Foreign.Ptr
import Data.Complex
import Foreign.Marshal.Utils
import Foreign.ForeignPtr

import Pipes
import FFTW

import SDR.Buffer

mkFFTWArray :: Int -> IO (ForeignPtr (Complex CDouble))
mkFFTWArray samples = do
    memory <- fftwMalloc (fromIntegral $ samples * sizeOf (undefined :: Complex CDouble))
    newForeignPtr_ memory

foreign import ccall unsafe "convertFFT"
    c_convertFFT :: CInt -> Ptr (Complex CDouble) -> Ptr (Complex CDouble) -> IO ()

convertFFT :: Int -> ForeignPtr (Complex CDouble) -> ForeignPtr (Complex CDouble) -> IO ()
convertFFT samples ina out = 
    withForeignPtr ina $ \ip -> 
    withForeignPtr out $ \op -> 
        c_convertFFT (fromIntegral samples) ip op

fftw :: Int -> IO (Pipe (ForeignPtr (Complex CDouble)) (ForeignPtr (Complex CDouble)) IO ())
fftw samples = do
    ina <- mallocForeignBufferAligned samples

    plan <- withForeignPtr ina $ \ip -> 
        planDFT1d samples ip ip fftwForward fftwEstimate
    
    return $ forever $ do
        out <- lift $ mallocForeignBufferAligned samples
        res <- await
        lift $ convertFFT samples res out
        lift $ withForeignPtr out $ \op -> 
            executeDFT plan op op
        yield out

fftwReal :: Int -> IO (Pipe (ForeignPtr CDouble) (ForeignPtr (Complex CDouble)) IO ())
fftwReal samples = do
    --Allocate in and out buffers that wont be used because there doesnt seem to be a way to create a plan without them
    ina <- mallocForeignBufferAligned samples
    out <- mallocForeignBufferAligned samples

    plan <- withForeignPtr ina $ \ip -> 
        withForeignPtr out $ \op -> 
            planDFTR2C1d samples ip op fftwEstimate

    return $ forever $ do
        out <- lift $ mallocForeignBufferAligned samples
        ina <- await
        lift $ withForeignPtr ina $ \ip -> 
            withForeignPtr out $ \op -> 
                executeDFTR2C plan ip op
        yield out

