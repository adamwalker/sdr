{-# LANGUAGE ScopedTypeVariables #-}

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
import           SDR.Util
import           SDR.CPUID

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
        inBufRTLSDR  :: VS.Vector CUChar
        inBufRTLSDR  =  VG.fromList $ take size [0 ..]
        inBufBladeRF :: VS.Vector CShort
        inBufBladeRF =  VG.fromList $ take size [0 ..]

        numConv   = 16386
        inBufConv :: VS.Vector CUChar
        inBufConv = VG.fromList $ take size $ concat $ repeat [0 .. 255]

        duplicate :: [a] -> [a]
        duplicate = concatMap func
            where func x = [x, x]

        coeffs2 :: VS.Vector Float
        coeffs2 =  VG.fromList $ duplicate $ take numCoeffs [0 ..]

    outBuf        :: VS.MVector RealWorld Float <- VGM.new size
    outBufComplex :: VS.MVector RealWorld (Complex Float) <- VGM.new size

    info <- getCPUInfo

    let hasFeatures :: [(CPUInfo -> Bool, a)] -> [a]
        hasFeatures = map snd . filter (($ info) . fst)

    resampler3 <- resampleCRR2   interpolation decimation coeffsList
    resampler4 <- resampleCSSERR interpolation decimation coeffsList
    resampler5 <- resampleCAVXRR interpolation decimation coeffsList

    resampler3C <- resampleCRC    interpolation decimation coeffsList
    resampler4C <- resampleCSSERC interpolation decimation coeffsList
    resampler5C <- resampleCAVXRC interpolation decimation coeffsList

    --Benchmarks
    defaultMain [
            bgroup "filter" [
                bgroup "real" $ hasFeatures [
                    (const True, bench "highLevel"   $ nfIO $ filterHighLevel          coeffs    num inBuf outBuf),
                    (const True, bench "imperative1" $ nfIO $ filterImperative1        coeffs    num inBuf outBuf),
                    (const True, bench "imperative2" $ nfIO $ filterImperative2        coeffs    num inBuf outBuf),
                    (const True, bench "c"           $ nfIO $ filterCRR                coeffs    num inBuf outBuf),
                    (hasSSE42,   bench "cSSE"        $ nfIO $ filterCSSERR             coeffs    num inBuf outBuf),
                    (hasSSE42,   bench "cSSESym"     $ nfIO $ filterCSSESymmetricRR    coeffsSym num inBuf outBuf),
                    (hasAVX,     bench "cAVX"        $ nfIO $ filterCAVXRR             coeffs    num inBuf outBuf),
                    (hasAVX,     bench "cAVXSym"     $ nfIO $ filterCAVXSymmetricRR    coeffsSym num inBuf outBuf)
                ],
                bgroup "complex" $ hasFeatures [
                    (const True, bench "highLevel"   $ nfIO $ filterHighLevel          coeffs    num inBufComplex outBufComplex),
                    (const True, bench "c"           $ nfIO $ filterCRC                coeffs    num inBufComplex outBufComplex),
                    (hasSSE42,   bench "cSSE"        $ nfIO $ filterCSSERC             coeffs2   num inBufComplex outBufComplex),
                    (hasSSE42,   bench "cSSE2"       $ nfIO $ filterCSSERC2            coeffs    num inBufComplex outBufComplex),
                    (hasSSE42,   bench "cSSESym"     $ nfIO $ filterCSSESymmetricRC    coeffsSym num inBufComplex outBufComplex),
                    (hasAVX,     bench "cAVX"        $ nfIO $ filterCAVXRC             coeffs2   num inBufComplex outBufComplex),
                    (hasAVX,     bench "cAVX2"       $ nfIO $ filterCAVXRC2            coeffs    num inBufComplex outBufComplex),
                    (hasAVX,     bench "cAVXSym"     $ nfIO $ filterCAVXSymmetricRC    coeffsSym num inBufComplex outBufComplex)
                ]
            ],
            bgroup "decimate" [
                bgroup "real" $ hasFeatures [
                    (const True, bench "highLevel"   $ nfIO $ decimateHighLevel        decimation coeffs    (num `quot` decimation) inBuf outBuf),
                    (const True, bench "c"           $ nfIO $ decimateCRR              decimation coeffs    (num `quot` decimation) inBuf outBuf),
                    (hasSSE42,   bench "cSSE"        $ nfIO $ decimateCSSERR           decimation coeffs    (num `quot` decimation) inBuf outBuf),
                    (hasSSE42,   bench "cSSESym"     $ nfIO $ decimateCSSESymmetricRR  decimation coeffsSym (num `quot` decimation) inBuf outBuf),
                    (hasAVX,     bench "cAVX"        $ nfIO $ decimateCAVXRR           decimation coeffs    (num `quot` decimation) inBuf outBuf),
                    (hasAVX,     bench "cAVXSym"     $ nfIO $ decimateCAVXSymmetricRR  decimation coeffsSym (num `quot` decimation) inBuf outBuf)
                ],
                bgroup "complex" $ hasFeatures [
                    (const True, bench "highLevel"   $ nfIO $ decimateHighLevel        decimation coeffs    (num `quot` decimation) inBufComplex outBufComplex),
                    (const True, bench "c"           $ nfIO $ decimateCRC              decimation coeffs    (num `quot` decimation) inBufComplex outBufComplex),
                    (hasSSE42,   bench "cSSE"        $ nfIO $ decimateCSSERC           decimation coeffs2   (num `quot` decimation) inBufComplex outBufComplex),
                    (hasSSE42,   bench "cSSE2"       $ nfIO $ decimateCSSERC2          decimation coeffs    (num `quot` decimation) inBufComplex outBufComplex),
                    (hasSSE42,   bench "cSSESym"     $ nfIO $ decimateCSSESymmetricRC  decimation coeffsSym (num `quot` decimation) inBufComplex outBufComplex),
                    (hasAVX,     bench "cAVX"        $ nfIO $ decimateCAVXRC           decimation coeffs2   (num `quot` decimation) inBufComplex outBufComplex),
                    (hasAVX,     bench "cAVX2"       $ nfIO $ decimateCAVXRC2          decimation coeffs    (num `quot` decimation) inBufComplex outBufComplex),
                    (hasAVX,     bench "cAVXSym"     $ nfIO $ decimateCAVXSymmetricRC  decimation coeffsSym (num `quot` decimation) inBufComplex outBufComplex)
                ]
            ],
            bgroup "resample" [
                bgroup "real" $ hasFeatures [
                    (const True, bench "highLevel"   $ nfIO $ resampleHighLevel        interpolation decimation coeffs 0 (num `quot` decimation) inBuf outBuf),
                    (const True, bench "c"           $ nfIO $ resampleCRR              (num `quot` decimation) interpolation decimation 0 coeffs inBuf outBuf),
                    (const True, bench "c2"          $ nfIO $ resampler3               (num `quot` decimation) 0 inBuf outBuf),
                    (hasSSE42,   bench "cSSE"        $ nfIO $ resampler4               (num `quot` decimation) 0 inBuf outBuf),
                    (hasAVX,     bench "cAVX"        $ nfIO $ resampler5               (num `quot` decimation) 0 inBuf outBuf)
                ],
                bgroup "complex" $ hasFeatures [
                    (const True, bench "highLevel"   $ nfIO $ resampleHighLevel        interpolation decimation coeffs 0 (num `quot` decimation) inBufComplex outBufComplex),
                    (const True, bench "c"           $ nfIO $ resampler3C               (num `quot` decimation) 0 inBufComplex outBufComplex),
                    (const True, bench "SSE"         $ nfIO $ resampler4C               (num `quot` decimation) 0 inBufComplex outBufComplex),
                    (const True, bench "AVX"         $ nfIO $ resampler5C               (num `quot` decimation) 0 inBufComplex outBufComplex)
                ]
            ],
            bgroup "scaling" $ hasFeatures [
                (const True, bench "c"               $ nfIO $ scaleC    0.3 inBuf outBuf),
                (hasSSE42,   bench "cSSE"            $ nfIO $ scaleCSSE 0.3 inBuf outBuf),
                (hasAVX,     bench "cAVX"            $ nfIO $ scaleCAVX 0.3 inBuf outBuf)
            ],
            bgroup "conversion" [
                bgroup "RTLSDR" $ hasFeatures [
                    (const True, bench "h"    $ nf (interleavedIQUnsigned256ToFloat :: VS.Vector CUChar -> VS.Vector (Complex Float)) inBufRTLSDR),
                    (const True, bench "c"    $ nf interleavedIQUnsignedByteToFloat    inBufRTLSDR),
                    (hasSSE42,   bench "cSSE" $ nf interleavedIQUnsignedByteToFloatSSE inBufRTLSDR),
                    (hasAVX2,    bench "cAVX" $ nf interleavedIQUnsignedByteToFloatAVX inBufRTLSDR)
                ],
                bgroup "BladeRF" $ hasFeatures [
                    (const True, bench "h"    $ nf (interleavedIQSigned2048ToFloat :: VS.Vector CShort -> VS.Vector (Complex Float))  inBufBladeRF),
                    (const True, bench "c"    $ nf interleavedIQSignedWordToFloat    inBufBladeRF),
                    (hasSSE42,   bench "cSSE" $ nf interleavedIQSignedWordToFloatSSE inBufBladeRF),
                    (hasAVX2,    bench "cAVX" $ nf interleavedIQSignedWordToFloatAVX inBufBladeRF)
                ]
            ]
        ]

main = theBench
