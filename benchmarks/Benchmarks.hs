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

    --Benchmarks
    defaultMain [
            bgroup "filter" [
                bgroup "real" [
                    bench "highLevel"   $ nfIO $ filterHighLevel        num coeffs inBuf outBuf,
                    bench "imperative1" $ nfIO $ filterImperative1      num coeffs inBuf outBuf,
                    bench "imperative2" $ nfIO $ filterImperative2      num coeffs inBuf outBuf,
                    bench "c"           $ nfIO $ filterCRR              num coeffs inBuf outBuf,
                    bench "cSSE"        $ nfIO $ filterCSSERR           num coeffs inBuf outBuf,
                    bench "cSSESym"     $ nfIO $ filterCSSESymmetricRR  num coeffsSym inBuf outBuf,
                    bench "cAVX"        $ nfIO $ filterCAVXRR           num coeffs inBuf outBuf
                    --bench "cAVXSym"     $ nfIO $ filterCAVXSymmetricRR  num coeffsSym inBuf outBuf
                ],
                bgroup "complex" [
                    bench "highLevel"   $ nfIO $ filterHighLevel        num coeffs  inBufComplex outBufComplex,
                    bench "c"           $ nfIO $ filterCRC              num coeffs  inBufComplex outBufComplex,
                    bench "cSSE"        $ nfIO $ filterCSSERC           num coeffs2 inBufComplex outBufComplex,
                    bench "cSSE2"       $ nfIO $ filterCSSERC2          num coeffs  inBufComplex outBufComplex,
                    bench "cAVX"        $ nfIO $ filterCAVXRC           num coeffs2 inBufComplex outBufComplex
                ]
            ],
            bgroup "decimate" [
                bgroup "real" [
                    bench "highLevel"   $ nfIO $ decimateHighLevel        (num `quot` decimation) decimation coeffs inBuf outBuf,
                    bench "c"           $ nfIO $ decimateCRR              (num `quot` decimation) decimation coeffs inBuf outBuf,
                    bench "cSSE"        $ nfIO $ decimateCSSERR           (num `quot` decimation) decimation coeffs inBuf outBuf,
                    --bench "cSSESym"     $ nfIO $ decimateCSSESymmetricRR  (num `quot` decimation) decimation coeffsSym inBuf outBuf,
                    bench "cAVX"        $ nfIO $ decimateCAVXRR           (num `quot` decimation) decimation coeffs inBuf outBuf
                    --bench "cAVXSym"     $ nfIO $ decimateCAVXSymmetricRR  (num `quot` decimation) decimation coeffsSym inBuf outBuf
                ],
                bgroup "complex" [
                    bench "highLevel"   $ nfIO $ decimateHighLevel      (num `quot` decimation) decimation coeffs  inBufComplex outBufComplex,
                    bench "c"           $ nfIO $ decimateCRC            (num `quot` decimation) decimation coeffs  inBufComplex outBufComplex,
                    bench "cSSE"        $ nfIO $ decimateCSSERC         (num `quot` decimation) decimation coeffs2 inBufComplex outBufComplex,
                    bench "cSSE2"       $ nfIO $ decimateCSSERC2        (num `quot` decimation) decimation coeffs  inBufComplex outBufComplex,
                    bench "cAVX"        $ nfIO $ decimateCAVXRC         (num `quot` decimation) decimation coeffs2 inBufComplex outBufComplex
                ]
            ]
        ]

main = theBench
