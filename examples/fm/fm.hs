import           Control.Monad.Trans.Either
import           Data.Vector.Generic        as VG 
import           Pipes
import qualified Pipes.Prelude              as P
import           Foreign.Storable.Complex

import SDR.Filter 
import SDR.RTLSDRStream
import SDR.Util
import SDR.Demod
import SDR.Pulse

--The filter coefficients are stored in another module
import Coeffs

samples    = 8192
frequency  = 105700000

main = eitherT putStrLn return $ do

    str  <- sdrStream frequency 1280000 1 (fromIntegral samples * 2)

    lift $ do

        sink <- pulseAudioSink

        deci <- fastDecimatorC 8 coeffsRFDecim 
        resp <- haskellResampler 3 10 coeffsAudioResampler
        filt <- fastSymmetricFilterR  coeffsAudioFilter

        runEffect $   str
                  >-> P.map convertCAVX 
                  >-> decimate deci samples 
                  >-> P.map (fmDemodVec 0) 
                  >-> resample resp samples 
                  >-> filterr filt samples
                  >-> P.map (VG.map (* 0.2)) 
                  >-> sink

