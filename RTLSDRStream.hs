module RTLSDRStream where

import Control.Monad
import Control.Monad.Trans.Either
import Data.Word
import Foreign.Storable
import Foreign.ForeignPtr
import Foreign.C.Types
import Data.Time.Clock

import Pipes
import RTLSDR

sdrStream :: Word32 -> Word32 -> Int -> EitherT String IO (Producer (ForeignPtr CUChar) IO ())
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

        tm <- getCurrentTime

        return $ mkSdrStream samples dev tm

mkSdrStream :: Int -> RTLSDR -> UTCTime -> Producer (ForeignPtr CUChar) IO ()
mkSdrStream samples dev tm = do
    buf  <- lift $ mallocForeignPtrArray (samples * 2)
    res' <- lift $ withForeignPtr buf $ \bp -> 
        readSync dev bp (samples * 2)
    tm' <- lift getCurrentTime
    lift $ putStrLn $ "Received packet. TS: " ++ show (diffUTCTime tm' tm)
    if res'==False then lift $ print "Stream terminated" else yield buf >> mkSdrStream samples dev tm'

