import Data.Complex
import Control.Monad.Trans.Either
import Pipes
import qualified Pipes.Prelude as P
import Foreign.C.Types
import Data.Array.Storable
import Foreign.ForeignPtr
import Foreign.Marshal.Array
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

coeffsAudioResampler :: [CDouble]
coeffsAudioResampler = [
  -2.9163e-02,
  -1.6141e-03,
  -1.6167e-03,
  -1.5996e-03,
  -1.5489e-03,
  -1.4769e-03,
  -1.3704e-03,
  -1.2432e-03,
  -1.0826e-03,
  -9.0308e-04,
  -6.9209e-04,
  -4.6510e-04,
  -2.1099e-04,
   5.7408e-05,
   3.4707e-04,
   6.4224e-04,
   9.5425e-04,
   1.2659e-03,
   1.5873e-03,
   1.8981e-03,
   2.2091e-03,
   2.5022e-03,
   2.7880e-03,
   3.0450e-03,
   3.2880e-03,
   3.4932e-03,
   3.6799e-03,
   3.8212e-03,
   3.9455e-03,
   4.0178e-03,
   4.0809e-03,
   4.0528e-03,
   3.9484e-03,
   3.8795e-03,
   3.7086e-03,
   3.5096e-03,
   3.2507e-03,
   2.9527e-03,
   2.6042e-03,
   2.2164e-03,
   1.7861e-03,
   1.3202e-03,
   8.2169e-04,
   2.9356e-04,
  -2.5787e-04,
  -8.2417e-04,
  -1.4040e-03,
  -1.9901e-03,
  -2.5740e-03,
  -3.1489e-03,
  -3.7056e-03,
  -4.2406e-03,
  -4.7427e-03,
  -5.2066e-03,
  -5.6215e-03,
  -5.9844e-03,
  -6.2836e-03,
  -6.5187e-03,
  -6.6780e-03,
  -6.7630e-03,
  -6.7570e-03,
  -6.6635e-03,
  -6.4585e-03,
  -6.1770e-03,
  -5.7915e-03,
  -5.2918e-03,
  -4.6978e-03,
  -3.9946e-03,
  -3.1919e-03,
  -2.2858e-03,
  -1.2824e-03,
  -1.8314e-04,
   1.0067e-03,
   2.2808e-03,
   3.6374e-03,
   5.0602e-03,
   6.5505e-03,
   8.0987e-03,
   9.6921e-03,
   1.1321e-02,
   1.2976e-02,
   1.4648e-02,
   1.6326e-02,
   1.7996e-02,
   1.9648e-02,
   2.1269e-02,
   2.2851e-02,
   2.4380e-02,
   2.5849e-02,
   2.7244e-02,
   2.8555e-02,
   2.9771e-02,
   3.0884e-02,
   3.1887e-02,
   3.2781e-02,
   3.3537e-02,
   3.4168e-02,
   3.4664e-02,
   3.5021e-02,
   3.5236e-02,
   3.5308e-02,
   3.5236e-02,
   3.5021e-02,
   3.4664e-02,
   3.4168e-02,
   3.3537e-02,
   3.2781e-02,
   3.1887e-02,
   3.0884e-02,
   2.9771e-02,
   2.8555e-02,
   2.7244e-02,
   2.5849e-02,
   2.4380e-02,
   2.2851e-02,
   2.1269e-02,
   1.9648e-02,
   1.7996e-02,
   1.6326e-02,
   1.4648e-02,
   1.2976e-02,
   1.1321e-02,
   9.6921e-03,
   8.0987e-03,
   6.5505e-03,
   5.0602e-03,
   3.6374e-03,
   2.2808e-03,
   1.0067e-03,
  -1.8314e-04,
  -1.2824e-03,
  -2.2858e-03,
  -3.1919e-03,
  -3.9946e-03,
  -4.6978e-03,
  -5.2918e-03,
  -5.7915e-03,
  -6.1770e-03,
  -6.4585e-03,
  -6.6635e-03,
  -6.7570e-03,
  -6.7630e-03,
  -6.6780e-03,
  -6.5187e-03,
  -6.2836e-03,
  -5.9844e-03,
  -5.6215e-03,
  -5.2066e-03,
  -4.7427e-03,
  -4.2406e-03,
  -3.7056e-03,
  -3.1489e-03,
  -2.5740e-03,
  -1.9901e-03,
  -1.4040e-03,
  -8.2417e-04,
  -2.5787e-04,
   2.9356e-04,
   8.2169e-04,
   1.3202e-03,
   1.7861e-03,
   2.2164e-03,
   2.6042e-03,
   2.9527e-03,
   3.2507e-03,
   3.5096e-03,
   3.7086e-03,
   3.8795e-03,
   3.9484e-03,
   4.0528e-03,
   4.0809e-03,
   4.0178e-03,
   3.9455e-03,
   3.8212e-03,
   3.6799e-03,
   3.4932e-03,
   3.2880e-03,
   3.0450e-03,
   2.7880e-03,
   2.5022e-03,
   2.2091e-03,
   1.8981e-03,
   1.5873e-03,
   1.2659e-03,
   9.5425e-04,
   6.4224e-04,
   3.4707e-04,
   5.7408e-05,
  -2.1099e-04,
  -4.6510e-04,
  -6.9209e-04,
  -9.0308e-04,
  -1.0826e-03,
  -1.2432e-03,
  -1.3704e-03,
  -1.4769e-03,
  -1.5489e-03,
  -1.5996e-03,
  -1.6167e-03,
  -1.6141e-03,
  -2.9163e-02
    ]

bufNum = 1
bufLen = 16384
samples = fromIntegral (bufNum * bufLen) `quot` 2
decimation = 8
sqd = samples `quot` decimation

main = eitherT putStrLn return $ do

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

    audioFFT       <- lift $ fftwReal sqd 
    audioSpectrum  <- plot ((sqd `quot` 2) + 1) (1/100)

    pulseSink      <- lift $ pulseAudioSink sqd

    --sampling frequency of fm demodulated signal is 160 khz
    --resampling factor is 48/160
    --resampling factor is 3/10
    --audio frequency cutoff is 15khz
    --which is ~0.1 of sampling frequency

    --Build the pipeline
    let inputSpectrum :: Producer (ForeignPtr (Complex CDouble)) IO ()
        inputSpectrum = str >-> P.mapM (makeComplexBuffer samples) >-> rfDecimator 

        spectrumFFTSink :: Consumer (ForeignPtr (Complex CDouble)) IO () 
        spectrumFFTSink = rfFFT >-> devnull

        p1 :: Producer (ForeignPtr (Complex CDouble)) IO () 
        p1 = runEffect $ fork inputSpectrum >-> hoist lift spectrumFFTSink

        demodulated :: Producer (ForeignPtr CDouble) IO ()
        demodulated = p1 >-> fmDemod sqd >-> audioResampler

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

