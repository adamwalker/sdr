import           Control.Monad.Trans.Except
import           Control.Error.Util
import           Data.Vector.Generic        as VG 
import           Pipes
import qualified Pipes.Prelude              as P

import SDR.Filter 
import SDR.RTLSDRStream
import SDR.Util
import SDR.Demod
import SDR.Pulse
import SDR.CPUID

--The filter coefficients are stored in another module
import Coeffs

samples    = 8192
frequency  = 105700000

main = exceptT putStrLn return $ do

    info <- lift getCPUInfo

    str  <- sdrStream (defaultRTLSDRParams frequency 1280000) 1 (fromIntegral samples * 2)

    lift $ do

        sink <- pulseAudioSink

        deci <- fastDecimatorC info 8 coeffsRFDecim 
        resp <- fastResamplerR info 3 10 coeffsAudioResampler
        filt <- fastFilterSymR info coeffsAudioFilter

        runEffect $   str
                  >-> P.map (interleavedIQUnsignedByteToFloatFast info)
                  >-> firDecimator deci samples 
                  >-> fmDemod
                  >-> firResampler resp samples 
                  >-> firFilter filt samples
                  >-> P.map (VG.map (* 0.2)) 
                  >-> sink

