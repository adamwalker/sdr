import Data.Complex
import Control.Monad.Trans.Either
import Pipes
import qualified Pipes.Prelude as P
import Foreign.C.Types
import Foreign.ForeignPtr
import Control.Monad

import Graphics.UI.GLFW as G

import Filter 
import RTLSDRStream
import Util
import FFT
import Plot
import Demod
import Pulse
import Buffer

--Designed in Octave with 'signal' package using remez algorithm
coeffsRFDecim :: [Complex CDouble]
coeffsRFDecim = [

   0.0329394,
   0.0083235,
   0.0067407,
   0.0032584,
  -0.0019575,
  -0.0082985,
  -0.0149386,
  -0.0207572,
  -0.0246124,
  -0.0253119,
  -0.0220131,
  -0.0141571,
  -0.0016430,
   0.0149006,
   0.0345188,
   0.0557522,
   0.0767567,
   0.0956371,
   0.1106267,
   0.1202691,
   0.1235811,
   0.1202691,
   0.1106267,
   0.0956371,
   0.0767567,
   0.0557522,
   0.0345188,
   0.0149006,
  -0.0016430,
  -0.0141571,
  -0.0220131,
  -0.0253119,
  -0.0246124,
  -0.0207572,
  -0.0149386,
  -0.0082985,
  -0.0019575,
   0.0032584,
   0.0067407,
   0.0083235,
   0.0329394

    ]

--In Octave:
--pkg load signal
--a = [1 1 0 0]
--f = [0 0.1 0.3 1]
--b = remez(30, f, a)

coeffsAudioResampler :: [CDouble]
coeffsAudioResampler = [

  -3.0862e-04,
   1.9752e-03,
   4.1096e-03,
   5.8306e-03,
   4.9003e-03,
  -4.7097e-04,
  -1.0215e-02,
  -2.1264e-02,
  -2.7609e-02,
  -2.2054e-02,
   7.7070e-04,
   4.1331e-02,
   9.3585e-02,
   1.4595e-01,
   1.8477e-01,
   1.9910e-01,
   1.8477e-01,
   1.4595e-01,
   9.3585e-02,
   4.1331e-02,
   7.7070e-04,
  -2.2054e-02,
  -2.7609e-02,
  -2.1264e-02,
  -1.0215e-02,
  -4.7097e-04,
   4.9003e-03,
   5.8306e-03,
   4.1096e-03,
   1.9752e-03,
  -3.0862e-04
    ]

--In Octave:
--pkg load signal
--a = [1 1 0 0]
--f = [0 0.3125 0.39 1]
--b = remez(50, f, a)

coeffsAudioFilter :: [CDouble]
coeffsAudioFilter = [

   1.4300e-03,
   7.7910e-03,
   3.6881e-04,
  -3.9208e-03,
  -5.7843e-03,
  -5.4785e-04,
   7.0511e-03,
   7.9887e-03,
  -9.8260e-04,
  -1.1368e-02,
  -1.0366e-02,
   4.0556e-03,
   1.7507e-02,
   1.2702e-02,
  -9.6383e-03,
  -2.6509e-02,
  -1.4798e-02,
   1.9937e-02,
   4.1280e-02,
   1.6460e-02,
  -4.2208e-02,
  -7.3911e-02,
  -1.7528e-02,
   1.2712e-01,
   2.8367e-01,
   3.5123e-01,
   2.8367e-01,
   1.2712e-01,
  -1.7528e-02,
  -7.3911e-02,
  -4.2208e-02,
   1.6460e-02,
   4.1280e-02,
   1.9937e-02,
  -1.4798e-02,
  -2.6509e-02,
  -9.6383e-03,
   1.2702e-02,
   1.7507e-02,
   4.0556e-03,
  -1.0366e-02,
  -1.1368e-02,
  -9.8260e-04,
   7.9887e-03,
   7.0511e-03,
  -5.4785e-04,
  -5.7843e-03,
  -3.9208e-03,
   3.6881e-04,
   7.7910e-03,
   1.4300e-03
    ]

bufNum = 1
bufLen = 16384
samples = fromIntegral (bufNum * bufLen) `quot` 2
decimation = 8
sqd = samples `quot` decimation

main = eitherT putStrLn return $ do

    --sampling frequency of input is 1280 khz
    --sampling frequency of fm demodulated signal is 160 khz
    --resampling factor is 48/160
    --resampling factor is 3/10
    --audio frequency cutoff is 19khz (0.3958)
    --start cutoff at 15khz (0.3125)

    --initialize glfw 
    lift $ setErrorCallback $ Just $ \error msg -> do
        print error
        putStrLn msg

    res <- lift $ G.init
    unless res (left "error initializing glfw")

    --Initialize the components that require initialization
    str            <- sdrStream 91100000 1280000 bufNum bufLen

    rfDecimator    <- lift $ decimateC decimation coeffsRFDecim samples sqd

    rfFFT          <- lift $ fftw sqd
    rfSpectrum     <- plot sqd (1 / fromIntegral sqd)

    audioResampler <- lift $ resampleR 3 10 coeffsAudioResampler sqd sqd

    audioFilter    <- lift $ filterR coeffsAudioFilter sqd sqd

    audioFFT       <- lift $ fftwReal sqd 
    audioSpectrum  <- plot ((sqd `quot` 2) + 1) (1/100)

    pulseSink      <- lift $ pulseAudioSink sqd

    --Build the pipeline
    let inputSpectrum :: Producer (ForeignPtr (Complex CDouble)) IO ()
        inputSpectrum = str >-> P.mapM (makeComplexBuffer samples) >-> rfDecimator 

        spectrumFFTSink :: Consumer (ForeignPtr (Complex CDouble)) IO () 
        spectrumFFTSink = rfFFT >-> devnull

        p1 :: Producer (ForeignPtr (Complex CDouble)) IO () 
        p1 = runEffect $ fork inputSpectrum >-> hoist lift spectrumFFTSink

        demodulated :: Producer (ForeignPtr CDouble) IO ()
        demodulated = p1 >-> fmDemod sqd >-> audioResampler >-> audioFilter

        audioSpectrumSink :: Consumer (ForeignPtr CDouble) IO ()
        audioSpectrumSink = audioFFT >-> audioSpectrum

        p2 :: Producer (ForeignPtr CDouble) IO ()
        p2 = runEffect $ fork demodulated >-> hoist lift audioSpectrumSink

        audioSink :: Consumer (ForeignPtr CDouble) IO ()
        audioSink = P.mapM (multiplyConstFF sqd 0.2) >-> P.mapM (doubleToFloat sqd) >-> rate sqd >-> pulseSink

        pipeline :: IO ()
        pipeline = runEffect $ p2 >-> audioSink

    --Run the pipeline
    lift pipeline

