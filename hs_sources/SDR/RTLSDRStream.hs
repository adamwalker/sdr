{-| Stream samples from a Realtek RTL2832U based device -}
module SDR.RTLSDRStream (
    sdrStream
    ) where

import           Control.Monad
import           Control.Monad.Trans.Either
import           Data.Word
import           Foreign.ForeignPtr
import           Foreign.C.Types
import           Control.Concurrent         hiding (yield)
import           Foreign.Marshal.Utils
import qualified Data.Vector.Storable       as VS

import           Pipes
import           Pipes.Concurrent 
import           RTLSDR

-- | Returns a producer that streams data from a Realtek RTL2832U based device. You probably want to use `makeComplexBufferVect` to turn it into a list of complex Floats.
sdrStream :: Word32                                                -- ^ Frequency
          -> Word32                                                -- ^ Sample rate
          -> Word32                                                -- ^ Number of buffers
          -> Word32                                                -- ^ Buffer length
          -> EitherT String IO (Producer (VS.Vector CUChar) IO ()) -- ^ Either a string describing the error that occurred or the Producer
sdrStream frequency sampleRate bufNum bufLen = do
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

        (output, input) <- spawn unbounded

        forkOS $ void $ readAsync dev bufNum bufLen $ \dat num -> void $ do
            let numBytes = fromIntegral $ bufNum * bufLen
            fp <- mallocForeignPtrArray numBytes
            withForeignPtr fp $ \fpp -> moveBytes fpp dat numBytes
            let v = VS.unsafeFromForeignPtr0 fp numBytes
            atomically (send output v)

        return $ fromInput input


