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

        async samples dev --mkSdrStream samples dev tm

mkSdrStream :: Int -> RTLSDR -> UTCTime -> Producer (ForeignPtr CUChar) IO ()
mkSdrStream samples dev tm = do
    buf  <- lift $ mallocForeignBufferAligned (samples * 2)
    res' <- lift $ withForeignPtr buf $ \bp -> 
        readSync dev bp (samples * 2)
    tm' <- lift getCurrentTime
    lift $ putStrLn $ "Received packet. TS: " ++ show (diffUTCTime tm' tm)
    if res'==False then lift $ print "Stream terminated" else yield buf >> mkSdrStream samples dev tm'

async :: Int -> RTLSDR -> IO (Producer (ForeignPtr CUChar) IO ())
async samples dev = do
    (output, input) <- spawn Unbounded
    forkOS $ doAsync dev output
    return $ fromInput input

doAsync dev output = void $ readAsync dev 0 0 $ \dat num -> void $ do
    print num 
    let numBytes = 262144
    fp <- mallocForeignBufferAligned numBytes
    withForeignPtr fp $ \fpp -> moveBytes fpp dat numBytes
    atomically (send output fp)

