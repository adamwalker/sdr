# SDR

A Software Defined Radio library written in Haskell

# Features
* Write software defined radio applications in Haskell
* Signal processing blocks can be chained together using the [Pipes](https://hackage.haskell.org/package/pipes) library
* Signal processing functions are implemented in both Haskell and C:
    * Optimised C implementations of signal processing functions that utilise SIMD instructions
    * Performance of Haskell signal processing functions within a factor of 2 of C (without SIMD) thanks to the vector library, stream fusion and ghc's LLVM backend
* Can filter, decimate and resample
* Helper functions for FIR filter design using window functions and plotting of the frequency response 
* FFTs using [FFTW](http://www.fftw.org)
* Line and waterfall plots using OpenGL
* FM demodulation
* PulseAudio sound sink
* [rtl-sdr](http://sdr.osmocom.org/trac/wiki/rtl-sdr) based radio source supported and other sources are easily added
* Extensive benchmark and test suites of signal processing functions

See https://github.com/adamwalker/sdr-apps for a collection of simple apps built on the library.

# Screenshot
A chunk of the FM broadcast spectrum. Captured with an RTLSDR device and drawn as a waterfall using the Plot module.

![Screenshot](../screenshots/screenshots/screenshot.png?raw=true)


# Getting Started

## Installation
Building with cabal sandboxes is recommended:

```
cabal sandbox init
git clone https://github.com/adamwalker/dynamic-graph
git clone https://github.com/adamwalker/haskell-fftw-simple
git clone https://github.com/adamwalker/sdr
cabal sandbox add-source dynamic-graph haskell-fftw-simple sdr
cabal install sdr
```

## Example Applications
Clone and build:

```
git clone https://github.com/adamwalker/sdr-apps  
cabal sandbox add-source sdr-apps
cabal install sdr-apps
```

To run the FM receiver:
```
.cabal-sandbox/bin/fm -f <your favourite station, e.g. 90.2M>  
```

To run the waterfall plot:
```
.cabal-sandbox/bin/waterfall -f <center frequency, e.g. 90.2M> -r <sample rate, e.g. 1280M>
```

# Usage

An FM receiver:

```haskell
import Control.Monad.Trans.Either
import Data.Vector.Generic as VG 
import Pipes
import qualified Pipes.Prelude as P
import Foreign.Storable.Complex

import SDR.Filter 
import SDR.RTLSDRStream
import SDR.Util
import SDR.Demod
import SDR.Pulse

--The filter coefficients are stored in another module
import Coeffs

bufLen     = 16384
samples    = fromIntegral bufLen `quot` 2
decimation = 8
sqd        = samples `quot` decimation
frequency  = 90200000

main = eitherT putStrLn return $ do

    str  <- sdrStream frequency 1280000 1 bufLen
    sink <- lift pulseAudioSink 

    lift $ runEffect $   str 
                     >-> P.map (makeComplexBufferVect samples) 
                     >-> decimate decimation (VG.fromList coeffsRFDecim) samples sqd 
                     >-> P.map (fmDemodVec 0) 
                     >-> resample 3 10 (VG.fromList coeffsAudioResampler) sqd sqd 
                     >-> filterr (VG.fromList coeffsAudioFilter) sqd sqd
                     >-> P.map (VG.map ((* 0.2))) 
                     >-> sink
```

# Disclaimer
I started this project to learn about signal processing. I still have no idea what I'm doing.

Only tested on Arch Linux.
