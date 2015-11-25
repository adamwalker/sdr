{-# LANGUAGE RecordWildCards #-}

{-| Stream samples from a Realtek RTL2832U based device -}
module SDR.RTLSDRStream (
    RTLSDRParams(..),
    defaultRTLSDRParams,
    setRTLSDRParams,
    sdrStream,
    sdrStreamFromDevice
    ) where

import           Control.Monad
import           Control.Monad.Trans.Either
import           Data.Word
import           Data.Int
import           Foreign.ForeignPtr
import           Foreign.C.Types
import           Control.Concurrent         hiding (yield)
import           Foreign.Marshal.Utils
import qualified Data.Vector.Storable       as VS

import           Pipes
import           Pipes.Concurrent 
import           RTLSDR

-- | RTLSDR configuration parameters
data RTLSDRParams = RTLSDRParams {
    centerFreq     :: Word32,
    sampleRate     :: Word32,
    freqCorrection :: Int32,
    tunerGain      :: Maybe Int32
}

-- | Some reasonable default parameters
defaultRTLSDRParams :: Word32       -- ^ Frequency
                    -> Word32       -- ^ Sample rate
                    -> RTLSDRParams 
defaultRTLSDRParams freq sampleRate = RTLSDRParams freq sampleRate 0 Nothing 

-- | Set the configuration parameters for a device
setRTLSDRParams :: RTLSDR       -- ^ Device handle
                -> RTLSDRParams -- ^ Parameters
                -> IO ()
setRTLSDRParams dev RTLSDRParams{..} = do
    setCenterFreq     dev centerFreq
    setSampleRate dev sampleRate
    setFreqCorrection dev freqCorrection
    case tunerGain of
        Nothing -> setTunerGainMode dev False
        Just g  -> setTunerGainMode dev True  >> setTunerGain dev g
    return ()

-- | Returns a producer that streams data from a Realtek RTL2832U based device. You probably want to use `interleavedIQUnsigned256ToFloat` to turn it into a list of complex Floats. This function initializes and configures the device for you. Use `sdrStreamFromDevice` if you need more control over how the device is configured or want to configure it yourself.
sdrStream :: RTLSDRParams                                          -- ^ Configuration parameters
          -> Word32                                                -- ^ Number of buffers
          -> Word32                                                -- ^ Buffer length
          -> EitherT String IO (Producer (VS.Vector CUChar) IO ()) -- ^ Either a string describing the error that occurred or the Producer
sdrStream params bufNum bufLen = do
    lift $ putStrLn "Initializing RTLSDR device..."

    dev' <- lift $ open 0
    dev  <- maybe (left "Failed to open device") return dev'

    lift $ do
        t <- getTunerType dev
        putStrLn $ "Found a: " ++ show t
        setRTLSDRParams dev params
        sdrStreamFromDevice dev bufNum bufLen

-- | Returns a producer that streams data from a Realtek RTL2832U based device. You probably want to use `interleavedIQUnsigned256ToFloat` to turn it into a list of complex Floats. This function takes a pre-configured device handle to stream from.
sdrStreamFromDevice :: RTLSDR                                 -- ^ Device handle
                    -> Word32                                 -- ^ Number of buffers
                    -> Word32                                 -- ^ Buffer length
                    -> IO (Producer (VS.Vector CUChar) IO ()) -- ^ The producer
sdrStreamFromDevice dev bufNum bufLen = do
    resetBuffer dev

    (output, input) <- spawn unbounded

    forkOS $ void $ readAsync dev bufNum bufLen $ \dat num -> void $ do
        let numBytes = fromIntegral $ bufNum * bufLen
        fp <- mallocForeignPtrArray numBytes
        withForeignPtr fp $ \fpp -> moveBytes fpp dat numBytes
        let v = VS.unsafeFromForeignPtr0 fp numBytes
        atomically (send output v)

    return $ fromInput input

