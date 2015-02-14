{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances, ScopedTypeVariables, BangPatterns #-}

import           Control.Monad.Primitive 
import           Control.Monad
import           Foreign.C.Types
import           Foreign.Ptr
import           Unsafe.Coerce

import qualified Data.Vector.Generic               as VG
import qualified Data.Vector.Generic.Mutable       as VGM
import qualified Data.Vector.Storable              as VS
import qualified Data.Vector.Storable.Mutable      as VSM
import qualified Data.Vector.Fusion.Stream         as VFS
import qualified Data.Vector.Fusion.Stream.Monadic as VFSM

import           Criterion.Main

-- | A class for things that can be multiplied by a scalar.
class Mult a b where
    mult :: a -> b -> a

instance (Num a) => Mult a a where
    mult = (*)

-- | Fill a mutable vector from a monadic stream
{-# INLINE fill #-}
fill :: (PrimMonad m, Functor m, VGM.MVector vm a) => VFS.MStream m a -> vm (PrimState m) a -> m ()
fill str outBuf = void $ VFSM.foldM' put 0 str
    where 
    put i x = do
        VGM.unsafeWrite outBuf i x
        return $ i + 1

-- | The functions to be benchmarked

filterHighLevel :: (PrimMonad m, Functor m, Num a, Mult a b, VG.Vector v a, VG.Vector v b, VGM.MVector vm a) => v b -> Int -> v a -> vm (PrimState m) a -> m ()
filterHighLevel coeffs num inBuf outBuf = fill (VFSM.generate num dotProd) outBuf
    where
    dotProd offset = VG.sum $ VG.zipWith mult (VG.unsafeDrop offset inBuf) coeffs

filterImperative1 :: (PrimMonad m, Functor m, Num a, Mult a b, VG.Vector v a, VG.Vector v b, VGM.MVector vm a) => v b -> Int -> v a -> vm (PrimState m) a -> m ()
filterImperative1 coeffs num inBuf outBuf = go 0
    where
    go offset 
        | offset < num = do
            let res = dotProd offset
            VGM.unsafeWrite outBuf offset res
            go $ offset + 1
        | otherwise    = return ()
    dotProd offset = VG.sum $ VG.zipWith mult (VG.unsafeDrop offset inBuf) coeffs

filterImperative2 :: (PrimMonad m, Functor m, Num a, Mult a b, VG.Vector v a, VG.Vector v b, VGM.MVector vm a) => v b -> Int -> v a -> vm (PrimState m) a -> m ()
filterImperative2 coeffs num inBuf outBuf = go 0
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

foreign import ccall unsafe "filterC"
    filterC_c :: CInt -> CInt -> Ptr CFloat -> Ptr CFloat -> Ptr CFloat -> IO ()

filterC :: Int -> VS.Vector Float -> VS.Vector Float -> VS.MVector RealWorld Float -> IO ()
filterC num coeffs inBuf outBuf = 
    VS.unsafeWith (unsafeCoerce coeffs) $ \cPtr -> 
        VS.unsafeWith (unsafeCoerce inBuf) $ \iPtr -> 
            VSM.unsafeWith (unsafeCoerce outBuf) $ \oPtr -> 
                filterC_c (fromIntegral num) (fromIntegral $ VG.length coeffs) cPtr iPtr oPtr

foreign import ccall unsafe "filterSSE"
    filterSSE_c :: CInt -> CInt -> Ptr CFloat -> Ptr CFloat -> Ptr CFloat -> IO ()

filterCSSE :: Int -> VS.Vector Float -> VS.Vector Float -> VS.MVector RealWorld Float -> IO ()
filterCSSE num coeffs inBuf outBuf = 
    VS.unsafeWith (unsafeCoerce coeffs) $ \cPtr -> 
        VS.unsafeWith (unsafeCoerce inBuf) $ \iPtr -> 
            VSM.unsafeWith (unsafeCoerce outBuf) $ \oPtr -> 
                filterSSE_c (fromIntegral num) (fromIntegral $ VG.length coeffs) cPtr iPtr oPtr

foreign import ccall unsafe "filterAVX"
    filterAVX_c :: CInt -> CInt -> Ptr CFloat -> Ptr CFloat -> Ptr CFloat -> IO ()

filterCAVX :: Int -> VS.Vector Float -> VS.Vector Float -> VS.MVector RealWorld Float -> IO ()
filterCAVX num coeffs inBuf outBuf = 
    VS.unsafeWith (unsafeCoerce coeffs) $ \cPtr -> 
        VS.unsafeWith (unsafeCoerce inBuf) $ \iPtr -> 
            VSM.unsafeWith (unsafeCoerce outBuf) $ \oPtr -> 
                filterAVX_c (fromIntegral num) (fromIntegral $ VG.length coeffs) cPtr iPtr oPtr

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

    filterHighLevel   coeffs num inBuf outBuf0
    filterImperative1 coeffs num inBuf outBuf1
    filterImperative2 coeffs num inBuf outBuf2
    filterC           num coeffs inBuf outBuf3
    filterCSSE        num coeffs inBuf outBuf4
    filterCAVX        num coeffs inBuf outBuf5

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
    let size      =  16384
        numCoeffs =  100
        num       =  size - numCoeffs + 1

        coeffs    :: VS.Vector Float
        coeffs    =  VG.fromList $ take numCoeffs [0 ..]
        inBuf     :: VS.Vector Float
        inBuf     =  VG.fromList $ take size [0 ..]

    outBuf        :: VS.MVector RealWorld Float <- VGM.new size

    --Benchmarks
    defaultMain [
            bgroup "bench" [
                bench "highLevel"   $ nfIO $ filterHighLevel   coeffs num inBuf outBuf,
                bench "imperative1" $ nfIO $ filterImperative1 coeffs num inBuf outBuf,
                bench "imperative2" $ nfIO $ filterImperative2 coeffs num inBuf outBuf,
                bench "c"           $ nfIO $ filterC           num coeffs inBuf outBuf,
                bench "cSSE"        $ nfIO $ filterCSSE        num coeffs inBuf outBuf,
                bench "cSSE"        $ nfIO $ filterCAVX        num coeffs inBuf outBuf
            ]
        ]

main = theBench
