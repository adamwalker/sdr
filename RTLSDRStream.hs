module RTLSDRStream where

import Control.Monad
import Control.Monad.Trans.Either
import Data.Word
import Foreign.Storable
import Data.Array.Storable
import Foreign.C.Types

import Pipes
import RTLSDR

sdrStream :: Word32 -> Word32 -> Int -> EitherT String IO (Producer (StorableArray Int CUChar) IO ())
sdrStream frequency sampleRate samples = do
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

        return $ mkSdrStream samples dev

mkSdrStream :: Int -> RTLSDR -> Producer (StorableArray Int CUChar) IO ()
mkSdrStream samples dev = do
    res' <- lift $ readSync dev (samples * 2)
    maybe (lift $ print "Stream terminated") (yield >=> const (mkSdrStream samples dev)) res'

