{-# LANGUAGE ScopedTypeVariables, BangPatterns #-}

import           Control.Monad.Primitive 
import           Control.Monad
import           Foreign.C.Types
import           Foreign.Ptr
import           Unsafe.Coerce
import           Data.Complex

import qualified Data.Vector.Generic               as VG
import qualified Data.Vector.Generic.Mutable       as VGM
import qualified Data.Vector.Storable              as VS
import qualified Data.Vector.Storable.Mutable      as VSM
import qualified Data.Vector.Fusion.Stream         as VFS
import qualified Data.Vector.Fusion.Stream.Monadic as VFSM

import           Foreign.Storable.Complex
import           Criterion.Main

import           SDR.FilterInternal

theBench :: IO ()
theBench = do
    --Setup
    let size       =  16384
        numCoeffs  =  128
        num        =  size - numCoeffs + 1
        decimation =  4
        interpolation = 3
        numCoeffsDiv2  =  64

        coeffsList :: [Float]
        coeffsList = take numCoeffs [0 ..]
        coeffs    :: VS.Vector Float
        coeffs    =  VG.fromList $ take numCoeffs [0 ..]
        coeffsSym    :: VS.Vector Float
        coeffsSym    =  VG.fromList $ take numCoeffsDiv2 [0 ..]
        inBuf     :: VS.Vector Float
        inBuf     =  VG.fromList $ take size [0 ..]
        inBufComplex :: VS.Vector (Complex Float)
        inBufComplex =  VG.fromList $ take size $ do
            i <- [0..]
            return $ i :+ i

        numConv   = 16386
        inBufConv :: VS.Vector CUChar
        inBufConv = VG.fromList $ take size $ concat $ repeat [0 .. 255]

        duplicate :: [a] -> [a]
        duplicate = concat . map func 
            where func x = [x, x]

        coeffs2 :: VS.Vector Float
        coeffs2 =  VG.fromList $ duplicate $ take numCoeffs [0 ..]

    outBuf        :: VS.MVector RealWorld Float <- VGM.new size
    outBufComplex :: VS.MVector RealWorld (Complex Float) <- VGM.new size

    resampler3 <- resampleCRR2   interpolation decimation coeffsList
    resampler4 <- resampleCSSERR interpolation decimation coeffsList
    resampler5 <- resampleCAVXRR interpolation decimation coeffsList

    --Benchmarks
    defaultMain [
            bgroup "filter" [
                bgroup "real" [
                    bench "highLevel"   $ nfIO $ filterHighLevel          coeffs    num inBuf outBuf,
                    bench "imperative1" $ nfIO $ filterImperative1        coeffs    num inBuf outBuf,
                    bench "imperative2" $ nfIO $ filterImperative2        coeffs    num inBuf outBuf,
                    bench "c"           $ nfIO $ filterCRR                coeffs    num inBuf outBuf,
                    bench "cSSE"        $ nfIO $ filterCSSERR             coeffs    num inBuf outBuf,
                    bench "cSSESym"     $ nfIO $ filterCSSESymmetricRR    coeffsSym num inBuf outBuf,
                    bench "cAVX"        $ nfIO $ filterCAVXRR             coeffs    num inBuf outBuf,
                    bench "cAVXSym"     $ nfIO $ filterCAVXSymmetricRR    coeffsSym num inBuf outBuf
                ],
                bgroup "complex" [
                    bench "highLevel"   $ nfIO $ filterHighLevel          coeffs    num inBufComplex outBufComplex,
                    bench "c"           $ nfIO $ filterCRC                coeffs    num inBufComplex outBufComplex,
                    bench "cSSE"        $ nfIO $ filterCSSERC             coeffs2   num inBufComplex outBufComplex,
                    bench "cSSE2"       $ nfIO $ filterCSSERC2            coeffs    num inBufComplex outBufComplex,
                    bench "cSSESym"     $ nfIO $ filterCSSESymmetricRC    coeffsSym num inBufComplex outBufComplex,
                    bench "cAVX"        $ nfIO $ filterCAVXRC             coeffs2   num inBufComplex outBufComplex,
                    bench "cAVX2"       $ nfIO $ filterCAVXRC2            coeffs    num inBufComplex outBufComplex
                ]
            ],
            bgroup "decimate" [
                bgroup "real" [
                    bench "highLevel"   $ nfIO $ decimateHighLevel        decimation coeffs    (num `quot` decimation) inBuf outBuf,
                    bench "c"           $ nfIO $ decimateCRR              decimation coeffs    (num `quot` decimation) inBuf outBuf,
                    bench "cSSE"        $ nfIO $ decimateCSSERR           decimation coeffs    (num `quot` decimation) inBuf outBuf,
                    bench "cSSESym"     $ nfIO $ decimateCSSESymmetricRR  decimation coeffsSym (num `quot` decimation) inBuf outBuf,
                    bench "cAVX"        $ nfIO $ decimateCAVXRR           decimation coeffs    (num `quot` decimation) inBuf outBuf,
                    bench "cAVXSym"     $ nfIO $ decimateCAVXSymmetricRR  decimation coeffsSym (num `quot` decimation) inBuf outBuf
                ],
                bgroup "complex" [
                    bench "highLevel"   $ nfIO $ decimateHighLevel        decimation coeffs    (num `quot` decimation) inBufComplex outBufComplex,
                    bench "c"           $ nfIO $ decimateCRC              decimation coeffs    (num `quot` decimation) inBufComplex outBufComplex,
                    bench "cSSE"        $ nfIO $ decimateCSSERC           decimation coeffs2   (num `quot` decimation) inBufComplex outBufComplex,
                    bench "cSSE2"       $ nfIO $ decimateCSSERC2          decimation coeffs    (num `quot` decimation) inBufComplex outBufComplex,
                    bench "cSSESym"     $ nfIO $ decimateCSSESymmetricRC  decimation coeffsSym (num `quot` decimation) inBufComplex outBufComplex,
                    bench "cAVX"        $ nfIO $ decimateCAVXRC           decimation coeffs2   (num `quot` decimation) inBufComplex outBufComplex,
                    bench "cAVX2"       $ nfIO $ decimateCAVXRC2          decimation coeffs    (num `quot` decimation) inBufComplex outBufComplex
                ]
            ],
            bgroup "resample" [
                bgroup "real" [
                    bench "highLevel"   $ nfIO $ resampleHighLevel        interpolation decimation coeffs 0 (num `quot` decimation) inBuf outBuf,
                    bench "c"           $ nfIO $ resampleCRR              (num `quot` decimation) interpolation decimation 0 coeffs inBuf outBuf,
                    bench "c2"          $ nfIO $ resampler3               (num `quot` decimation) 0 inBuf outBuf,
                    bench "cSSE"        $ nfIO $ resampler4               (num `quot` decimation) 0 inBuf outBuf,
                    bench "cAVX"        $ nfIO $ resampler5               (num `quot` decimation) 0 inBuf outBuf
                ],
                bgroup "complex" [
                    bench "highLevel"   $ nfIO $ resampleHighLevel        interpolation decimation coeffs 0 (num `quot` decimation) inBufComplex outBufComplex
                ]
            ]
        ]

main = theBench
