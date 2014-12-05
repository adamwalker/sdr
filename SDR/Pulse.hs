module SDR.Pulse where

import Foreign.ForeignPtr
import Foreign.C.Types
import Control.Monad
import Control.Concurrent
import Data.ByteString.Internal
import Data.Vector.Storable as VS

import Sound.Pulse.Simple
import Pipes
import Pipes.Concurrent

-- | Returns a consumer that sends all incoming data to pulseaudio.
pulseAudioSink :: IO (Consumer (VS.Vector CFloat) IO ())
pulseAudioSink = do
    (output, input) <- spawn Unbounded
    doIt <- doPulse 
    forkOS $ runEffect $ fromInput input >-> doIt
    return $ toOutput output

doPulse :: IO (Consumer (VS.Vector CFloat) IO ())
doPulse = do
    s <- simpleNew Nothing "example" Play Nothing "this is an example application" (SampleSpec (F32 LittleEndian) 48000 1) Nothing Nothing
    return $ for cat $ \buf -> 
        lift $ do
            let (fp, offset, length) = VS.unsafeToForeignPtr buf
            simpleWriteRaw s (PS (castForeignPtr fp) (offset * 4) (length * 4))

