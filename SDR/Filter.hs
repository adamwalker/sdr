{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances #-}
module SDR.Filter where

import Control.Monad
import Control.Monad.Trans.Either
import Data.Bits
import Data.Word
import Data.Array.MArray
import Foreign.Storable
import Foreign.Marshal.Array
import Foreign.Marshal.Alloc
import Foreign.Marshal.Utils
import Foreign.C.Types
import Data.Complex
import Foreign.ForeignPtr
import Foreign.Storable.Complex
import Foreign.Ptr
import Control.Exception 
import qualified Data.Vector.Generic as VG
import qualified Data.Vector.Generic.Mutable as VGM
import qualified Data.Vector.Storable as VS
import qualified Data.Vector.Storable.Mutable as VSM

import Pipes

import SDR.Buffer

class Mult a b where
    mult :: a -> b -> a

instance (Num a) => Mult a a where
    mult = (*)

instance (Num a) => Mult (Complex a) a where
    mult (x :+ y) z = (x * z) :+ (y * z)

data Buffer v a = Buffer {
    buffer :: v a,
    offset :: Int,
    size   :: Int
}

newBuffer :: Storable a => Int -> IO (Buffer VSM.IOVector a)
newBuffer size = do
    buf <- VGM.new size
    return $ Buffer buf 0 size

advanceOutBuf :: Storable a => Int -> Buffer VSM.IOVector a -> Int -> Pipe b (VS.Vector a) IO (Buffer VSM.IOVector a)
advanceOutBuf blockSizeOut (Buffer bufOut offsetOut spaceOut) count = 
    if count == spaceOut then do
        bufOutF <- lift $ VS.unsafeFreeze bufOut
        yield bufOutF
        outBuf' <- lift $ VGM.new blockSizeOut
        return $ Buffer outBuf' 0 blockSizeOut
    else 
        return $ Buffer bufOut (offsetOut + count) (spaceOut - count) 

--Filtering
{-# INLINE filterOne #-}
filterOne :: (Num a, Mult a b, Storable a, Storable b) => Int -> VS.Vector b -> Int -> Int -> VS.Vector a -> Int -> VSM.IOVector a -> IO ()
filterOne coeffsLength coeffs num inOffset inBuf outOffset outBuf = fill 0
    where
    fill i 
        | i < num = do
            let dp = dotProd (i + inOffset)
            VGM.unsafeWrite outBuf (i + outOffset) dp
            fill (i + 1)
        | otherwise = return ()
    {-# INLINE dotProd #-}
    dotProd offset = VG.sum $ VG.zipWith mult (VG.unsafeSlice offset (VG.length coeffs) inBuf) coeffs

{-# INLINE filterCross #-}
filterCross :: (Num a, Mult a b, Storable a, Storable b) => Int -> VS.Vector b -> Int -> Int -> Int -> VS.Vector a -> VS.Vector a -> Int -> VSM.IOVector a -> IO ()
filterCross coeffsLength coeffs numInput num lastOffset lastBuf nextBuf outOffset outBuf = fill 0
    where
    fill i 
        | i < num = do
            let dp = dotProd i
            VGM.unsafeWrite outBuf (i + outOffset) dp
            fill (i + 1)
        | otherwise  = return ()
    {-# INLINE dotProd #-}
    dotProd i =   VG.sum (VG.zipWith mult (VG.unsafeSlice (i + lastOffset) (numInput - i) lastBuf) coeffs)
                + VG.sum (VG.zipWith mult nextBuf (VG.unsafeSlice i (VG.length coeffs - i) coeffs))

{-# SPECIALIZE INLINE filterr :: VS.Vector CDouble -> Int -> Int -> IO (Pipe (VS.Vector CDouble) (VS.Vector CDouble) IO ()) #-}
{-# SPECIALIZE INLINE filterr :: VS.Vector (Complex CDouble) -> Int -> Int -> IO (Pipe (VS.Vector (Complex CDouble)) (VS.Vector (Complex CDouble)) IO ()) #-}
{-# SPECIALIZE INLINE filterr :: VS.Vector CDouble -> Int -> Int -> IO (Pipe (VS.Vector (Complex CDouble)) (VS.Vector (Complex CDouble)) IO ()) #-}
filterr :: (Storable a, Storable b, Num a, Mult a b) => VS.Vector b -> Int -> Int -> IO (Pipe (VS.Vector a) (VS.Vector a) IO ())
filterr coeffs blockSizeIn blockSizeOut = do
    return $ filter' (VG.length coeffs) coeffs
    where 
    filter' numCoeffs coeffs = do
        inBuf  <- await
        outBuf <- lift $ newBuffer blockSizeOut
        simple (Buffer inBuf 0 blockSizeIn) outBuf 

        where

        simple (Buffer bufIn offsetIn spaceIn) bufferOut@(Buffer bufOut offsetOut spaceOut) = do
            let count = min (spaceIn - numCoeffs + 1) spaceOut
            lift $ filterOne numCoeffs coeffs count offsetIn bufIn offsetOut bufOut

            bufferOut' <- advanceOutBuf blockSizeOut bufferOut count

            let spaceIn'  = spaceIn - count
                offsetIn' = offsetIn + count

            case spaceIn' < numCoeffs of
                False -> simple (Buffer bufIn offsetIn' spaceIn') bufferOut'
                True  -> do
                    next <- await
                    crossover (Buffer bufIn offsetIn' spaceIn') next bufferOut'

        crossover (Buffer bufLast offsetLast spaceLast) bufNext bufferOut@(Buffer bufOut offsetOut spaceOut) = do
            let count = min (spaceLast - 1) spaceOut
            lift $ filterCross numCoeffs coeffs spaceLast count offsetLast bufLast bufNext offsetOut bufOut

            bufferOut' <- advanceOutBuf blockSizeOut bufferOut count

            case spaceLast - 1 == count of 
                True  -> simple (Buffer bufNext 0 blockSizeIn) bufferOut'
                False -> crossover (Buffer bufLast (offsetLast + count) (spaceLast - count)) bufNext bufferOut'

--Decimation
{-# INLINE decimateOne #-}
decimateOne :: (Num a, Mult a b, Storable a, Storable b) => Int -> Int -> VS.Vector b -> Int -> Int -> VS.Vector a -> Int -> VSM.IOVector a -> IO ()
decimateOne factor coeffsLength coeffs num inOffset inBuf outOffset outBuf = fill 0 0
    where 
    fill i j
        | i < num = do
            let dp = dotProd (j + inOffset)
            VGM.unsafeWrite outBuf (i + outOffset) dp
            fill (i + 1) (j + factor)
        | otherwise = return ()
    {-# INLINE dotProd #-}
    dotProd offset = VG.sum $ VG.zipWith mult (VG.unsafeSlice offset (VG.length coeffs) inBuf) coeffs

{-# INLINE decimateCross #-}
decimateCross :: (Num a, Mult a b, Storable a, Storable b) => Int -> Int -> VS.Vector b -> Int -> Int -> Int -> VS.Vector a -> VS.Vector a -> Int -> VSM.IOVector a -> IO ()
decimateCross factor coeffsLength coeffs numInput num lastOffset lastBuf nextBuf outOffset outBuf = fill 0 0
    where
    fill i j
        | i < num = do
            let dp = dotProd j
            VGM.unsafeWrite outBuf (i + outOffset) dp
            fill (i + 1) (j + factor)
        | otherwise  = return ()
    {-# INLINE dotProd #-}
    dotProd i =   VG.sum (VG.zipWith mult (VG.unsafeSlice (i + lastOffset) (numInput - i) lastBuf) coeffs)
                + VG.sum (VG.zipWith mult nextBuf (VG.unsafeSlice i (VG.length coeffs - i) coeffs))

{-# SPECIALIZE INLINE decimate :: Int -> VS.Vector CDouble -> Int -> Int -> IO (Pipe (VS.Vector CDouble) (VS.Vector CDouble) IO ()) #-}
{-# SPECIALIZE INLINE decimate :: Int -> VS.Vector (Complex CDouble) -> Int -> Int -> IO (Pipe (VS.Vector (Complex CDouble)) (VS.Vector (Complex CDouble)) IO ()) #-}
{-# SPECIALIZE INLINE decimate :: Int -> VS.Vector CDouble -> Int -> Int -> IO (Pipe (VS.Vector (Complex CDouble)) (VS.Vector (Complex CDouble)) IO ()) #-}
decimate :: (Storable a, Storable b, Mult a b, Num a) => Int -> VS.Vector b -> Int -> Int -> IO (Pipe (VS.Vector a) (VS.Vector a) IO ())
decimate factor coeffs blockSizeIn blockSizeOut = do
    return $ decimate' (VG.length coeffs) coeffs
    where
    decimate' numCoeffs coeffs = do
        inBuf  <- await
        outBuf <- lift $ newBuffer blockSizeOut
        simple (Buffer inBuf 0 blockSizeIn) outBuf

        where

        simple (Buffer bufIn offsetIn spaceIn) bufferOut@(Buffer bufOut offsetOut spaceOut) = do

            assert (spaceIn >= numCoeffs) $ return ()

            let count = min (((spaceIn - numCoeffs) `quot` factor) + 1) spaceOut
            lift $ decimateOne factor numCoeffs coeffs count offsetIn bufIn offsetOut bufOut

            bufferOut' <- advanceOutBuf blockSizeOut bufferOut count

            let spaceIn'  = spaceIn - count * factor
                offsetIn' = offsetIn + count * factor

            case spaceIn' < numCoeffs of
                False -> simple (Buffer bufIn offsetIn' spaceIn') bufferOut'
                True  -> do
                    next <- await
                    crossover (Buffer bufIn offsetIn' spaceIn') next bufferOut'

        crossover (Buffer bufLast offsetLast spaceLast) bufNext bufferOut@(Buffer bufOut offsetOut spaceOut) = do

            assert (spaceLast < numCoeffs) $ return ()

            let count = min (((spaceLast - 1) `quot` factor) + 1) spaceOut
            lift $ decimateCross factor numCoeffs coeffs spaceLast count offsetLast bufLast bufNext offsetOut bufOut

            bufferOut' <- advanceOutBuf blockSizeOut bufferOut count

            case ((spaceLast - 1) `quot` factor) + 1 == count of 
                True  -> simple (Buffer bufNext (offsetLast + count * factor - blockSizeIn) (blockSizeIn - (offsetLast + count * factor - blockSizeIn))) bufferOut'
                False -> crossover (Buffer bufLast (offsetLast + count * factor) (spaceLast - count * factor)) bufNext bufferOut'

--Rational resampling
type ResampleSingleC a = CInt -> CInt -> CInt -> Ptr a -> CInt -> CInt -> Ptr a -> Ptr a -> IO CInt
type ResampleCrossC  a = CInt -> CInt -> CInt -> Ptr a -> CInt -> CInt -> CInt -> Ptr a -> Ptr a -> Ptr a -> IO CInt

foreign import ccall unsafe "resample_onebuf_c"
    c_resampleOneBufC   :: ResampleSingleC (Complex CDouble)

foreign import ccall unsafe "resample_crossbuf_c"
    c_resampleCrossBufC :: ResampleCrossC (Complex CDouble)

foreign import ccall unsafe "resample_onebuf_r"
    c_resampleOneBufR   :: ResampleSingleC CDouble

foreign import ccall unsafe "resample_crossbuf_r"
    c_resampleCrossBufR :: ResampleCrossC CDouble

type ResampleSingle a = Int -> Int -> Int -> VS.Vector a -> Int -> Int -> Int -> VS.Vector a -> Int -> VSM.IOVector a -> IO Int
type ResampleCross  a = Int -> Int -> Int -> VS.Vector a -> Int -> Int -> Int -> Int -> VS.Vector a -> VS.Vector a -> Int -> VSM.IOVector a -> IO Int

resampleOneBuf :: Storable a => ResampleSingleC a -> ResampleSingle a
resampleOneBuf cfunc interpolation decimation coeffsLength coeffs filterOffset count inOffset inBuf outOffset outBuf = liftM fromIntegral $ 
    VS.unsafeWith  coeffs $ \cp -> 
    VS.unsafeWith  inBuf  $ \ip -> 
    VSM.unsafeWith outBuf $ \op -> 
        cfunc (fromIntegral interpolation) 
              (fromIntegral decimation) 
              (fromIntegral coeffsLength) 
              cp 
              (fromIntegral filterOffset) 
              (fromIntegral count) 
              (advancePtr ip inOffset)
              (advancePtr op outOffset)

resampleCrossBuf :: Storable a => ResampleCrossC a -> ResampleCross a
resampleCrossBuf cfunc interpolation decimation coeffsLength coeffs filterOffset numInput count lastOffset lastBuf nextBuf outOffset outBuf = liftM fromIntegral $ 
    VS.unsafeWith  coeffs  $ \cp -> 
    VS.unsafeWith  lastBuf $ \lp -> 
    VS.unsafeWith  nextBuf $ \np -> 
    VSM.unsafeWith outBuf  $ \op -> 
        cfunc (fromIntegral interpolation) 
              (fromIntegral decimation) 
              (fromIntegral coeffsLength) 
              cp 
              (fromIntegral filterOffset) 
              (fromIntegral numInput) 
              (fromIntegral count) 
              (advancePtr lp lastOffset)
              np 
              (advancePtr op outOffset)

resampleOneBufC   = resampleOneBuf   c_resampleOneBufC
resampleCrossBufC = resampleCrossBuf c_resampleCrossBufC
resampleOneBufR   = resampleOneBuf   c_resampleOneBufR
resampleCrossBufR = resampleCrossBuf c_resampleCrossBufR

quotUp q d = (q + (d - 1)) `quot` d

resample :: (Storable a) => ResampleSingle a -> ResampleCross a -> Int -> Int -> VS.Vector a -> Int -> Int -> IO (Pipe (VS.Vector a) (VS.Vector a) IO ())
resample single cross interpolation decimation coeffs blockSizeIn blockSizeOut = do
    return $ resample' (VS.length coeffs) coeffs
    where
    resample' numCoeffs coeffs = do
        inBuf  <- await
        outBuf <- lift $ newBuffer blockSizeOut
        simple (Buffer inBuf 0 blockSizeIn) outBuf 0

        where

        simple (Buffer bufIn offsetIn spaceIn) bufferOut@(Buffer bufOut offsetOut spaceOut) filterOffset = do
            --Check consistency
            assert (spaceIn * interpolation >= numCoeffs - filterOffset) $ return ()
            assert (offsetIn + spaceIn == blockSizeIn) $ return ()
            --available number of samples == interpolation * num_input
            --required number of samples  == decimation * (num_output - 1) + filter_length - filter_offset
            let count = min (((spaceIn * interpolation - numCoeffs + filterOffset) `quot` decimation) + 1) spaceOut
            --Run filter
            endOffset <- lift $ single interpolation decimation numCoeffs coeffs filterOffset count offsetIn bufIn offsetOut bufOut
            --Check consistency
            assert ((count * decimation + endOffset - filterOffset) `rem` interpolation == 0) $ return ()
            --Advance the output buffer
            bufferOut' <- advanceOutBuf blockSizeOut bufferOut count
            --samples no longer needed starting from filterOffset == count * decimation - filterOffset
            --inputs lying in this region                         == (count * decimation - filterOffset) / interpolation (rounding up)
            let usedInput = (count * decimation - filterOffset) `quotUp` interpolation 
                spaceIn'  = spaceIn  - usedInput
                offsetIn' = offsetIn + usedInput

            case spaceIn' * interpolation < numCoeffs - endOffset of
                False -> do
                    simple (Buffer bufIn offsetIn' spaceIn') bufferOut' endOffset
                True  -> do
                    next <- await
                    case spaceIn' == 0 of
                        True ->  simple    (Buffer next 0 blockSizeIn) bufferOut' endOffset
                        False -> crossover (Buffer bufIn offsetIn' spaceIn') next bufferOut' endOffset

        crossover (Buffer bufLast offsetLast spaceLast) bufNext bufferOut@(Buffer bufOut offsetOut spaceOut) filterOffset = do
            --Check conssitency
            assert (spaceLast > 0) $ return ()
            assert (spaceLast * interpolation < numCoeffs - filterOffset) $ return ()
            assert (offsetLast + spaceLast == blockSizeIn) $ return ()
            --outputsComputable is the number of outputs that need to be computed for the last buffer to no longer be needed
            --outputsComputable * decimation == numInput * interpolation + filterOffset + k
            let outputsComputable = (spaceLast * interpolation + filterOffset) `quotUp` decimation
                count = min outputsComputable spaceOut
            assert (count /= 0) $ return ()
            --Run the filter
            endOffset <- lift $ cross interpolation decimation numCoeffs coeffs filterOffset spaceLast count offsetLast bufLast bufNext offsetOut bufOut
            --Check consistency
            assert ((count * decimation + endOffset - filterOffset) `rem` interpolation == 0) $ return ()
            --Advance the output buffer
            bufferOut' <- advanceOutBuf blockSizeOut bufferOut count

            let inputUsed = (count * decimation - filterOffset) `quotUp` interpolation

            case inputUsed >= spaceLast of 
                True  -> simple (Buffer bufNext (offsetLast + inputUsed - blockSizeIn) (2 * blockSizeIn - (offsetLast + inputUsed))) bufferOut' endOffset
                False -> crossover (Buffer bufLast (offsetLast + inputUsed) (spaceLast - inputUsed)) bufNext bufferOut' endOffset

resampleC = resample resampleOneBufC resampleCrossBufC
resampleR = resample resampleOneBufR resampleCrossBufR

