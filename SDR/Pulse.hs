module SDR.Pulse where

import Foreign.ForeignPtr
import Foreign.Marshal.Array
import Foreign.C.Types
import Control.Monad
import Control.Concurrent
import Data.ByteString.Internal

import Sound.Pulse.Simple

import Pipes
import Pipes.Concurrent

pulseAudioSink :: Int -> IO (Consumer (ForeignPtr CFloat) IO ())
pulseAudioSink samples = do
    (output, input) <- spawn Unbounded
    doIt <- doPulse samples
    forkOS $ runEffect $ fromInput input >-> doIt
    return $ toOutput output

doPulse :: Int -> IO (Consumer (ForeignPtr CFloat) IO ())
doPulse samples = do
    s <- simpleNew Nothing "example" Play Nothing "this is an example application" (SampleSpec (F32 LittleEndian) 48000 1) Nothing Nothing
    return $ forever $ do
        buf <- await
        lift $ do
            --simpleDrain s
            simpleWriteRaw s (PS (castForeignPtr buf) 0 (samples * 4))
