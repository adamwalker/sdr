module RTLSDRStream where

import Control.Monad
import Control.Monad.Trans.Either
import Data.Word
import Foreign.Storable
import Foreign.ForeignPtr
import Foreign.C.Types
import Data.Time.Clock
import Control.Concurrent hiding (yield)
import Foreign.Marshal.Utils

import Pipes
import Pipes.Concurrent 
import RTLSDR

import Buffer

--Streams buffers with 131072 samples
sdrStream :: Word32 -> Word32 -> EitherT String IO (Producer (ForeignPtr CUChar) IO ())
sdrStream frequency sampleRate = do
    lift $ putStrLn "Initializing RTLSDR device"

    dev' <- lift $ open 0
    dev  <- maybe (left "Failed to open device") return dev'

    lift $ do
        t <- getTunerType dev
        putStrLn $ "Found a: " ++ show t

        setFreqCorrection dev 0
        setSampleRate dev sampleRate
        setCenterFreq dev frequency
        setTunerGainMode dev False

        resetBuffer dev

        (output, input) <- spawn Unbounded

        forkOS $ void $ readAsync dev 0 0 $ \dat num -> void $ do
            print num 
            let numBytes = 262144
            fp <- mallocForeignBufferAligned numBytes
            withForeignPtr fp $ \fpp -> moveBytes fpp dat numBytes
            atomically (send output fp)

        return $ fromInput input


