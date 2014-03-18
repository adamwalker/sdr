module Pulse where

import Foreign.ForeignPtr
import Foreign.Marshal.Array
import Foreign.C.Types
import Control.Monad

import Sound.Pulse.Simple

import Pipes
import Pipes.Concurrent

pulseAudioSink :: Int -> IO (Consumer (ForeignPtr CDouble) IO ())
pulseAudioSink samples = do
    (output, input) <- spawn Unbounded
    doIt <- doPulse samples
    forkIO $ runEffect $ fromInput input >-> doIt
    return $ toOutput output


doPulse :: Int -> IO (Consumer (ForeignPtr CDouble) IO ())
doPulse samples = do
    s <- simpleNew Nothing "example" Play Nothing "this is an example application" (SampleSpec (F32 LittleEndian) 48000 1) Nothing Nothing
    return $ forever $ do
        buf <- await
        lift $ do
            dat <- withForeignPtr buf $ peekArray samples
            simpleWrite s (map (realToFrac . (* 0.1)) dat :: [Float])
