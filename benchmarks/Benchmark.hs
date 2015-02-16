{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances, ScopedTypeVariables, BangPatterns #-}

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

-- | A class for things that can be multiplied by a scalar.
class Mult a b where
    mult :: a -> b -> a

instance (Num a) => Mult a a where
    mult = (*)

instance (Num a) => Mult (Complex a) a where
    mult (x :+ y) z = (x * z) :+ (y * z)

-- | Fill a mutable vector from a monadic stream
{-# INLINE fill #-}
fill :: (PrimMonad m, Functor m, VGM.MVector vm a) => VFS.MStream m a -> vm (PrimState m) a -> m ()
fill str outBuf = void $ VFSM.foldM' put 0 str
    where 
    put i x = do
        VGM.unsafeWrite outBuf i x
        return $ i + 1

-- | The functions to be benchmarked

-- | Filters

filterHighLevel :: (PrimMonad m, Functor m, Num a, Mult a b, VG.Vector v a, VG.Vector v b, VGM.MVector vm a) => Int -> v b -> v a -> vm (PrimState m) a -> m ()
filterHighLevel num coeffs inBuf outBuf = fill (VFSM.generate num dotProd) outBuf
    where
    dotProd offset = VG.sum $ VG.zipWith mult (VG.unsafeDrop offset inBuf) coeffs

filterImperative1 :: (PrimMonad m, Functor m, Num a, Mult a b, VG.Vector v a, VG.Vector v b, VGM.MVector vm a) => Int -> v b -> v a -> vm (PrimState m) a -> m ()
filterImperative1 num coeffs inBuf outBuf = go 0
    where
    go offset 
        | offset < num = do
            let res = dotProd offset
            VGM.unsafeWrite outBuf offset res
            go $ offset + 1
        | otherwise    = return ()
    dotProd offset = VG.sum $ VG.zipWith mult (VG.unsafeDrop offset inBuf) coeffs

filterImperative2 :: (PrimMonad m, Functor m, Num a, Mult a b, VG.Vector v a, VG.Vector v b, VGM.MVector vm a) => Int -> v b -> v a -> vm (PrimState m) a -> m ()
filterImperative2 num coeffs inBuf outBuf = go 0
    where
    go offset 
        | offset < num = do
            let res = dotProd (VG.unsafeDrop offset inBuf)
            VGM.unsafeWrite outBuf offset res
            go $ offset + 1
        | otherwise    = return ()
    dotProd buf = go 0 0
        where
        go !accum j 
            | j < VG.length coeffs = go (VG.unsafeIndex buf j `mult` VG.unsafeIndex coeffs j  + accum) (j + 1)
            | otherwise            = accum

foreign import ccall unsafe "filterRR"
    filterRR_c :: CInt -> CInt -> Ptr CFloat -> Ptr CFloat -> Ptr CFloat -> IO ()

filterCRR :: Int -> VS.Vector Float -> VS.Vector Float -> VS.MVector RealWorld Float -> IO ()
filterCRR num coeffs inBuf outBuf = 
    VS.unsafeWith (unsafeCoerce coeffs) $ \cPtr -> 
        VS.unsafeWith (unsafeCoerce inBuf) $ \iPtr -> 
            VSM.unsafeWith (unsafeCoerce outBuf) $ \oPtr -> 
                filterRR_c (fromIntegral num) (fromIntegral $ VG.length coeffs) cPtr iPtr oPtr

foreign import ccall unsafe "filterRC"
    filterRC_c :: CInt -> CInt -> Ptr CFloat -> Ptr CFloat -> Ptr CFloat -> IO ()

filterCRC :: Int -> VS.Vector Float -> VS.Vector (Complex Float) -> VS.MVector RealWorld (Complex Float) -> IO ()
filterCRC num coeffs inBuf outBuf = 
    VS.unsafeWith (unsafeCoerce coeffs) $ \cPtr -> 
        VS.unsafeWith (unsafeCoerce inBuf) $ \iPtr -> 
            VSM.unsafeWith (unsafeCoerce outBuf) $ \oPtr -> 
                filterRC_c (fromIntegral num) (fromIntegral $ VG.length coeffs) cPtr iPtr oPtr

foreign import ccall unsafe "filterSSERR"
    filterSSE_c :: CInt -> CInt -> Ptr CFloat -> Ptr CFloat -> Ptr CFloat -> IO ()

filterCSSERR :: Int -> VS.Vector Float -> VS.Vector Float -> VS.MVector RealWorld Float -> IO ()
filterCSSERR num coeffs inBuf outBuf = 
    VS.unsafeWith (unsafeCoerce coeffs) $ \cPtr -> 
        VS.unsafeWith (unsafeCoerce inBuf) $ \iPtr -> 
            VSM.unsafeWith (unsafeCoerce outBuf) $ \oPtr -> 
                filterSSE_c (fromIntegral num) (fromIntegral $ VG.length coeffs) cPtr iPtr oPtr

foreign import ccall unsafe "filterAVXRR"
    filterAVX_c :: CInt -> CInt -> Ptr CFloat -> Ptr CFloat -> Ptr CFloat -> IO ()

filterCAVXRR :: Int -> VS.Vector Float -> VS.Vector Float -> VS.MVector RealWorld Float -> IO ()
filterCAVXRR num coeffs inBuf outBuf = 
    VS.unsafeWith (unsafeCoerce coeffs) $ \cPtr -> 
        VS.unsafeWith (unsafeCoerce inBuf) $ \iPtr -> 
            VSM.unsafeWith (unsafeCoerce outBuf) $ \oPtr -> 
                filterAVX_c (fromIntegral num) (fromIntegral $ VG.length coeffs) cPtr iPtr oPtr


-- | Decimation

decimateHighLevel :: (PrimMonad m, Functor m, Num a, Mult a b, VG.Vector v a, VG.Vector v b, VGM.MVector vm a) => Int -> Int -> v b -> v a -> vm (PrimState m) a -> m ()
decimateHighLevel num factor coeffs inBuf outBuf = fill x outBuf
    where 
    x = VFSM.map dotProd (VFSM.iterateN num (+ factor) 0)
    dotProd offset = VG.sum $ VG.zipWith mult (VG.unsafeDrop offset inBuf) coeffs

foreign import ccall unsafe "decimateRR"
    decimateC_c :: CInt -> CInt -> CInt -> Ptr CFloat -> Ptr CFloat -> Ptr CFloat -> IO ()

decimateCRR :: Int -> Int -> VS.Vector Float -> VS.Vector Float -> VS.MVector RealWorld Float -> IO ()
decimateCRR num factor coeffs inBuf outBuf = 
    VS.unsafeWith (unsafeCoerce coeffs) $ \cPtr -> 
        VS.unsafeWith (unsafeCoerce inBuf) $ \iPtr -> 
            VSM.unsafeWith (unsafeCoerce outBuf) $ \oPtr -> 
                decimateC_c (fromIntegral num) (fromIntegral factor) (fromIntegral $ VG.length coeffs) cPtr iPtr oPtr

foreign import ccall unsafe "decimateRC"
    decimateCRC_c :: CInt -> CInt -> CInt -> Ptr CFloat -> Ptr CFloat -> Ptr CFloat -> IO ()

decimateCRC :: Int -> Int -> VS.Vector Float -> VS.Vector (Complex Float) -> VS.MVector RealWorld (Complex Float) -> IO ()
decimateCRC num factor coeffs inBuf outBuf = 
    VS.unsafeWith (unsafeCoerce coeffs) $ \cPtr -> 
        VS.unsafeWith (unsafeCoerce inBuf) $ \iPtr -> 
            VSM.unsafeWith (unsafeCoerce outBuf) $ \oPtr -> 
                decimateCRC_c (fromIntegral num) (fromIntegral factor) (fromIntegral $ VG.length coeffs) cPtr iPtr oPtr

foreign import ccall unsafe "decimateSSERR"
    decimateSSE_c :: CInt -> CInt -> Int -> Ptr CFloat -> Ptr CFloat -> Ptr CFloat -> IO ()

decimateCSSERR :: Int -> Int -> VS.Vector Float -> VS.Vector Float -> VS.MVector RealWorld Float -> IO ()
decimateCSSERR num factor coeffs inBuf outBuf = 
    VS.unsafeWith (unsafeCoerce coeffs) $ \cPtr -> 
        VS.unsafeWith (unsafeCoerce inBuf) $ \iPtr -> 
            VSM.unsafeWith (unsafeCoerce outBuf) $ \oPtr -> 
                decimateSSE_c (fromIntegral num) (fromIntegral factor) (fromIntegral $ VG.length coeffs) cPtr iPtr oPtr

foreign import ccall unsafe "decimateAVXRR"
    decimateAVX_c :: CInt -> CInt -> CInt -> Ptr CFloat -> Ptr CFloat -> Ptr CFloat -> IO ()

decimateCAVXRR :: Int -> Int -> VS.Vector Float -> VS.Vector Float -> VS.MVector RealWorld Float -> IO ()
decimateCAVXRR num factor coeffs inBuf outBuf = 
    VS.unsafeWith (unsafeCoerce coeffs) $ \cPtr -> 
        VS.unsafeWith (unsafeCoerce inBuf) $ \iPtr -> 
            VSM.unsafeWith (unsafeCoerce outBuf) $ \oPtr -> 
                decimateAVX_c (fromIntegral num) (fromIntegral factor) (fromIntegral $ VG.length coeffs) cPtr iPtr oPtr

-- | Conversion

foreign import ccall unsafe "convertC"
    convertC_c :: CInt -> Ptr CUChar -> Ptr CFloat -> IO ()

convertC :: Int -> VS.Vector CUChar -> VS.MVector RealWorld Float -> IO ()
convertC num inBuf outBuf = 
    VS.unsafeWith inBuf $ \iPtr -> 
        VSM.unsafeWith (unsafeCoerce outBuf) $ \oPtr -> 
            convertC_c (fromIntegral num) iPtr oPtr

convertHighLevel :: VS.Vector CUChar -> VS.Vector Float 
convertHighLevel = VG.map fromIntegral

test = do
    --Setup
    let size      =  8192
        numCoeffs =  100
        num       =  size - numCoeffs + 1

        coeffs    :: VS.Vector Float
        coeffs    =  VG.fromList $ take numCoeffs [0 ..]
        inBuf     :: VS.Vector Float
        inBuf     =  VG.fromList $ take size [0 ..]

    outBuf0       :: VS.MVector RealWorld Float <- VGM.new size
    outBuf1       :: VS.MVector RealWorld Float <- VGM.new size
    outBuf2       :: VS.MVector RealWorld Float <- VGM.new size
    outBuf3       :: VS.MVector RealWorld Float <- VGM.new size
    outBuf4       :: VS.MVector RealWorld Float <- VGM.new size
    outBuf5       :: VS.MVector RealWorld Float <- VGM.new size

    filterHighLevel   num coeffs inBuf outBuf0
    filterImperative1 num coeffs inBuf outBuf1
    filterImperative2 num coeffs inBuf outBuf2
    filterCRR         num coeffs inBuf outBuf3
    filterCSSERR      num coeffs inBuf outBuf4
    filterCAVXRR      num coeffs inBuf outBuf5

    out0 :: VS.Vector Float <- VG.freeze outBuf0
    out1 :: VS.Vector Float <- VG.freeze outBuf1
    out2 :: VS.Vector Float <- VG.freeze outBuf2
    out3 :: VS.Vector Float <- VG.freeze outBuf3
    out4 :: VS.Vector Float <- VG.freeze outBuf4
    out5 :: VS.Vector Float <- VG.freeze outBuf5

    print $ zip (VG.toList out0) (VG.toList out5)

theBench :: IO ()
theBench = do
    --Setup
    let size       =  16384
        numCoeffs  =  100
        num        =  size - numCoeffs + 1
        decimation =  4

        coeffs    :: VS.Vector Float
        coeffs    =  VG.fromList $ take numCoeffs [0 ..]
        inBuf     :: VS.Vector Float
        inBuf     =  VG.fromList $ take size [0 ..]
        inBufComplex :: VS.Vector (Complex Float)
        inBufComplex =  VG.fromList $ take size $ do
            i <- [0..]
            return $ i :+ i

        numConv   = 16386
        inBufConv :: VS.Vector CUChar
        inBufConv = VG.fromList $ take size $ concat $ repeat [0 .. 255]

    outBuf        :: VS.MVector RealWorld Float <- VGM.new size
    outBufComplex :: VS.MVector RealWorld (Complex Float) <- VGM.new size

    --Benchmarks
    defaultMain [
            bgroup "filterReal" [
                bench "highLevel"   $ nfIO $ filterHighLevel   num coeffs inBuf outBuf,
                bench "imperative1" $ nfIO $ filterImperative1 num coeffs inBuf outBuf,
                bench "imperative2" $ nfIO $ filterImperative2 num coeffs inBuf outBuf,
                bench "c"           $ nfIO $ filterCRR         num coeffs inBuf outBuf,
                bench "cSSE"        $ nfIO $ filterCSSERR      num coeffs inBuf outBuf,
                bench "cAVX"        $ nfIO $ filterCAVXRR      num coeffs inBuf outBuf
            ],
            bgroup "filterComplex" [
                bench "highLevel"   $ nfIO $ filterHighLevel   num coeffs inBufComplex outBufComplex,
                bench "c"           $ nfIO $ filterCRC         num coeffs inBufComplex outBufComplex
            ],
            bgroup "decimateReal" [
                bench "highLevel"   $ nfIO $ decimateHighLevel (num `quot` decimation) decimation coeffs inBuf outBuf,
                bench "c"           $ nfIO $ decimateCRR       (num `quot` decimation) decimation coeffs inBuf outBuf,
                bench "cSSE"        $ nfIO $ decimateCSSERR    (num `quot` decimation) decimation coeffs inBuf outBuf,
                bench "cAVX"        $ nfIO $ decimateCAVXRR    (num `quot` decimation) decimation coeffs inBuf outBuf
            ],
            bgroup "decimateComplex" [
                bench "highLevel"   $ nfIO $ decimateHighLevel (num `quot` decimation) decimation coeffs inBufComplex outBufComplex,
                bench "c"           $ nfIO $ decimateCRC       (num `quot` decimation) decimation coeffs inBufComplex outBufComplex
            ],
            bgroup "conversion" [
                bench "c"           $ nfIO $ convertC          numConv inBufConv outBuf
                --bench "c"           $ nfIO $ convertHighLevel  inBufConv
            ]
        ]

main = theBench
