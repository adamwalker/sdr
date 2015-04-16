{-# LANGUAGE FlexibleContexts, BangPatterns, ScopedTypeVariables, MultiParamTypeClasses, FlexibleInstances #-}

{-| Various utiliy signal processing functions -}
module SDR.Util (
    -- * Classes
    Mult,
    mult,

    -- * Conversion to Floating Point
    makeComplexBufferVect,
    convertC, 
    convertCSSE,
    convertCAVX,

    -- * Scaling
    scaleC,
    scaleCSSE,
    scaleCAVX
    ) where

import           Foreign.C.Types
import           Data.Complex
import           Data.Vector.Generic          as VG   hiding ((++))
import qualified Data.Vector.Generic.Mutable  as VGM
import           Data.Vector.Storable         as VS   hiding ((++))
import           Data.Vector.Storable.Mutable as VSM  
import           Control.Monad.Primitive
import           Unsafe.Coerce
import           Foreign.Ptr
import           System.IO.Unsafe
import           Foreign.Storable.Complex

-- | A class for things that can be multiplied by a scalar.
class Mult a b where
    mult :: a -> b -> a

instance (Num a) => Mult a a where
    mult = (*)

instance (Num a) => Mult (Complex a) a where
    mult (x :+ y) z = (x * z) :+ (y * z)

--TODO: none of these functions need the num argument

-- | Create a vector of complex float samples from a vector of interleaved I Q component bytes.
{-# INLINE makeComplexBufferVect #-}
makeComplexBufferVect :: (Num a, Integral a, Num b, Fractional b, VG.Vector v1 a, VG.Vector v2 (Complex b)) => Int -> v1 a -> v2 (Complex b)
makeComplexBufferVect samples input = VG.generate samples convert
    where
    {-# INLINE convert #-}
    convert idx  = convert' (input `VG.unsafeIndex` (2 * idx)) :+ convert' (input `VG.unsafeIndex` (2 * idx + 1))
    {-# INLINE convert' #-}
    convert' val = (fromIntegral val - 128) / 128

foreign import ccall unsafe "convertC"
    convertC_c :: CInt -> Ptr CUChar -> Ptr CFloat -> IO ()

-- | Same as `makeComplexBufferVect` but written in C
convertC :: Int -> VS.Vector CUChar -> VS.Vector (Complex Float)
convertC num inBuf = unsafePerformIO $ do
    outBuf <- VGM.new num
    VS.unsafeWith inBuf $ \iPtr -> 
        VSM.unsafeWith (unsafeCoerce outBuf) $ \oPtr -> 
            convertC_c (2 * fromIntegral num) iPtr oPtr
    VG.freeze outBuf

foreign import ccall unsafe "convertCSSE"
    convertCSSE_c :: CInt -> Ptr CUChar -> Ptr CFloat -> IO ()

-- | Same as `makeComplexBufferVect` but written in C using SSE intrinsics
convertCSSE :: Int -> VS.Vector CUChar -> VS.Vector (Complex Float)
convertCSSE num inBuf = unsafePerformIO $ do
    outBuf <- VGM.new num
    VS.unsafeWith inBuf $ \iPtr -> 
        VSM.unsafeWith (unsafeCoerce outBuf) $ \oPtr -> 
            convertCSSE_c (2 * fromIntegral num) iPtr oPtr
    VG.freeze outBuf

foreign import ccall unsafe "convertCAVX"
    convertCAVX_c :: CInt -> Ptr CUChar -> Ptr CFloat -> IO ()

-- | Same as `makeComplexBufferVect` but written in C using AVX intrinsics
convertCAVX :: Int -> VS.Vector CUChar -> VS.Vector (Complex Float)
convertCAVX num inBuf = unsafePerformIO $ do
    outBuf <- VGM.new num
    VS.unsafeWith inBuf $ \iPtr -> 
        VSM.unsafeWith (unsafeCoerce outBuf) $ \oPtr -> 
            convertCAVX_c (2 * fromIntegral num) iPtr oPtr
    VG.freeze outBuf

-- | Scaling
foreign import ccall unsafe "scale"
    scale_c :: CInt -> CFloat -> Ptr CFloat -> Ptr CFloat -> IO ()

-- | Scale a vector, written in C
scaleC :: Int -> Float -> VS.Vector Float -> VS.MVector RealWorld Float -> IO ()
scaleC num factor inBuf outBuf = 
    VS.unsafeWith (unsafeCoerce inBuf) $ \iPtr -> 
        VS.unsafeWith (unsafeCoerce outBuf) $ \oPtr -> 
            scale_c (fromIntegral num) (unsafeCoerce factor) iPtr oPtr

foreign import ccall unsafe "scaleSSE"
    scaleSSE_c :: CInt -> CFloat -> Ptr CFloat -> Ptr CFloat-> IO ()

-- | Scale a vector, written in C using SSE intrinsics
scaleCSSE :: Int -> Float -> VS.Vector Float -> VS.MVector RealWorld Float -> IO ()
scaleCSSE num factor inBuf outBuf = 
    VS.unsafeWith (unsafeCoerce inBuf) $ \iPtr -> 
        VS.unsafeWith (unsafeCoerce outBuf) $ \oPtr -> 
            scaleSSE_c (fromIntegral num) (unsafeCoerce factor) iPtr oPtr

foreign import ccall unsafe "scaleAVX"
    scaleAVX_c :: CInt -> CFloat -> Ptr CFloat -> Ptr CFloat -> IO ()

-- | Scale a vector, written in C using AVX intrinsics
scaleCAVX :: Int -> Float -> VS.Vector Float -> VS.MVector RealWorld Float -> IO ()
scaleCAVX num factor inBuf outBuf = 
    VS.unsafeWith (unsafeCoerce inBuf) $ \iPtr -> 
        VS.unsafeWith (unsafeCoerce outBuf) $ \oPtr -> 
            scaleAVX_c (fromIntegral num) (unsafeCoerce factor) iPtr oPtr

