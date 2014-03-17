module FFT where

import Control.Monad
import Data.Bits
import Foreign.Storable
import Foreign.C.Types
import Foreign.Ptr
import Data.Complex
import Data.Array.Storable
import Foreign.Marshal.Utils
import Foreign.ForeignPtr

import Data.Array.CArray
import Data.Array.CArray.Base

import Pipes
import FFTW

mkFFTWArray :: Int -> IO (IOCArray Int (Complex CDouble))
mkFFTWArray samples = do
    memory <- fftwMalloc (fromIntegral $ samples * sizeOf (undefined :: Complex CDouble))
    fp <- newForeignPtr_ memory
    unsafeForeignPtrToIOCArray fp (0, samples - 1) :: IO (IOCArray Int (Complex CDouble))

foreign import ccall unsafe "convertFFT"
    c_convertFFT :: CInt -> Ptr (Complex CDouble) -> Ptr (Complex CDouble) -> IO ()

convertFFT :: Int -> StorableArray Int (Complex CDouble) -> IOCArray Int (Complex CDouble) -> IO ()
convertFFT samples ina out = 
    withStorableArray ina $ \ip -> 
    withIOCArray      out $ \op -> 
        c_convertFFT (fromIntegral samples) ip op

convertForFFT :: Int -> IOCArray Int (Complex CDouble) -> Pipe (StorableArray Int (Complex CDouble)) (IOCArray Int (Complex CDouble)) IO ()
convertForFFT samples out = forever $ do
    res <- await
    lift $ convertFFT samples res out
    yield out

fftw :: Int -> IOCArray Int (Complex CDouble) -> IO (Pipe (StorableArray Int (Complex CDouble)) (IOCArray Int (Complex CDouble)) IO ())
fftw samples array = do
    plan <- withIOCArray array $ \ptr -> 
        planDFT1d samples ptr ptr fftwForward fftwEstimate
    
    return $ forever $ do
        res <- await
        lift $ convertFFT samples res array
        lift $ execute plan
        yield array

mkFFTWArrayReal :: Int -> IO (IOCArray Int CDouble)
mkFFTWArrayReal samples = do
    memory <- fftwMalloc (fromIntegral $ samples * sizeOf (undefined :: CDouble))
    fp <- newForeignPtr_ memory
    unsafeForeignPtrToIOCArray fp (0, samples - 1) :: IO (IOCArray Int CDouble)

fftwReal :: Int -> IOCArray Int CDouble -> IOCArray Int (Complex CDouble) -> IO (Pipe (StorableArray Int CDouble) (IOCArray Int (Complex CDouble)) IO ())
fftwReal samples ina out = do
    plan <- withIOCArray ina $ \ip -> 
        withIOCArray out $ \op -> 
            planDFTR2C1d samples ip op (1 `shiftL` 6)

    return $ forever $ do
        res <- await
        lift $ withStorableArray res $ \ip -> 
            withIOCArray ina $ \op -> 
                moveBytes op ip (samples * sizeOf (undefined :: CDouble))
        lift $ execute plan
        yield out

