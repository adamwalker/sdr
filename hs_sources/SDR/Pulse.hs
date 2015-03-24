{-| Pulse Audio Pipes sink -}
module SDR.Pulse (
    pulseAudioSink,
    doPulse
    ) where

import Foreign.ForeignPtr
import Foreign.C.Types
import Control.Monad
import Control.Concurrent
import Data.ByteString.Internal
import Data.Vector.Storable as VS

import Sound.Pulse.Simple
import Pipes
import Pipes.Concurrent

-- | Returns a consumer that sends all incoming data to pulseaudio. Runs Pulse Audio output writing in a different thread. This is probably what you want as it does not block the entire pipline while the data is being played.
pulseAudioSink :: IO (Consumer (VS.Vector CFloat) IO ())
pulseAudioSink = do
    (output, input) <- spawn Single
    doIt <- doPulse 
    forkOS $ runEffect $ fromInput input >-> doIt
    return $ toOutput output

-- | Returns a consumer that sends all incoming data to pulseaudio.
doPulse :: IO (Consumer (VS.Vector CFloat) IO ())
doPulse = do
    s <- simpleNew Nothing "Haskell SDR" Play Nothing "Software Defined Radio library" (SampleSpec (F32 LittleEndian) 48000 1) Nothing Nothing
    return $ for cat $ \buf -> 
        lift $ do
            let (fp, offset, length) = VS.unsafeToForeignPtr buf
            simpleWriteRaw s (PS (castForeignPtr fp) (offset * 4) (length * 4))

