module FFT where

import Control.Monad
import Data.Bits
import Foreign.Storable
import Foreign.C.Types
import Foreign.Ptr
import Data.Complex
import Foreign.Marshal.Utils
import Foreign.ForeignPtr

import Pipes
import FFTW

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

convertForFFT :: Int -> ForeignPtr (Complex CDouble) -> Pipe (ForeignPtr (Complex CDouble)) (ForeignPtr (Complex CDouble)) IO ()
convertForFFT samples out = forever $ do
    res <- await
    lift $ convertFFT samples res out
    yield out

fftw :: Int -> ForeignPtr (Complex CDouble) -> IO (Pipe (ForeignPtr (Complex CDouble)) (ForeignPtr (Complex CDouble)) IO ())
fftw samples array = do
    plan <- withForeignPtr array $ \ptr -> 
        planDFT1d samples ptr ptr fftwForward fftwEstimate
    
    return $ forever $ do
        res <- await
        lift $ convertFFT samples res array
        lift $ execute plan
        yield array

mkFFTWArrayReal :: Int -> IO (ForeignPtr CDouble)
mkFFTWArrayReal samples = do
    memory <- fftwMalloc (fromIntegral $ samples * sizeOf (undefined :: CDouble))
    newForeignPtr_ memory

fftwReal :: Int -> ForeignPtr CDouble -> ForeignPtr (Complex CDouble) -> IO (Pipe (ForeignPtr CDouble) (ForeignPtr (Complex CDouble)) IO ())
fftwReal samples ina out = do
    plan <- withForeignPtr ina $ \ip -> 
        withForeignPtr out $ \op -> 
            planDFTR2C1d samples ip op fftwEstimate

    return $ forever $ do
        res <- await
        lift $ withForeignPtr res $ \ip -> 
            withForeignPtr ina $ \op -> 
                moveBytes op ip (samples * sizeOf (undefined :: CDouble))
        lift $ execute plan
        yield out

