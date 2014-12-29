# SDR

A Software Defined Radio library written in Haskell

Features:
* Written in Haskell
* Performance close to C thanks to the vector library, stream fusion and ghc's LLVM backend
* Signal processing blocks can be chained together using the pipes library
* Can filter, decimate and resample
* FFTs using FFTW
* Line and waterfall plots using OpenGL
* FM demodulation
* PulseAudio sound sink
* rtl-sdr based radio source supported and other sources are easily added

See https://github.com/adamwalker/sdr-demo for a demo FM receiver
