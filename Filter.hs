module Filter where

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

import Buffer

import Pipes

assert msg False x = error $ "Assertion failed: " ++ msg
assert msg True  x = x

--Filtering
type FilterSingleC a = CInt -> Ptr a -> CInt -> Ptr a -> Ptr a -> IO ()
type FilterCrossC  a = CInt -> Ptr a -> CInt -> CInt -> Ptr a -> Ptr a -> Ptr a -> IO ()

foreign import ccall unsafe "filter_onebuf_c"
    c_filterOneBufC   :: FilterSingleC (Complex CDouble)

foreign import ccall unsafe "filter_crossbuf_c"
    c_filterCrossBufC :: FilterCrossC (Complex CDouble)

foreign import ccall unsafe "filter_onebuf_r"
    c_filterOneBufR   :: FilterSingleC CDouble

foreign import ccall unsafe "filter_crossbuf_r"
    c_filterCrossBufR :: FilterCrossC CDouble

type FilterSingle a = Int -> ForeignPtr a -> Int -> Int -> ForeignPtr a -> Int -> ForeignPtr a -> IO ()
type FilterCross  a = Int -> ForeignPtr a -> Int -> Int -> Int -> ForeignPtr a -> ForeignPtr a -> Int -> ForeignPtr a -> IO ()

filterOneBuf :: Storable a => FilterSingleC a -> FilterSingle a
filterOneBuf cfunc coeffsLength coeffs num inOffset inBuf outOffset outBuf = 
    withForeignPtr coeffs $ \cp -> 
    withForeignPtr inBuf  $ \ip -> 
    withForeignPtr outBuf $ \op -> 
        cfunc (fromIntegral coeffsLength) 
              cp 
              (fromIntegral num) 
              (advancePtr ip inOffset) 
              (advancePtr op outOffset)

filterCrossBuf :: Storable a => FilterCrossC a -> FilterCross a
filterCrossBuf cfunc coeffsLength coeffs numInput num lastOffset lastBuf nextBuf outOffset outBuf = 
    withForeignPtr coeffs  $ \cp -> 
    withForeignPtr lastBuf $ \lp -> 
    withForeignPtr nextBuf $ \np -> 
    withForeignPtr outBuf  $ \op -> 
        cfunc (fromIntegral coeffsLength) 
              cp 
              (fromIntegral numInput) 
              (fromIntegral num) 
              (advancePtr lp lastOffset) 
              np 
              (advancePtr op outOffset)

filterOneBufC   = filterOneBuf   c_filterOneBufC
filterCrossBufC = filterCrossBuf c_filterCrossBufC
filterOneBufR   = filterOneBuf   c_filterOneBufR
filterCrossBufR = filterCrossBuf c_filterCrossBufR

filterr :: (Storable a) => FilterSingle a -> FilterCross a -> Int -> ForeignPtr a -> Int -> Int -> Pipe (ForeignPtr a) (ForeignPtr a) IO ()
filterr single cross numCoeffs coeffs blockSizeIn blockSizeOut = do
    inBuf  <- await
    outBuf <- lift $ mallocForeignBufferAligned blockSizeOut

    simple inBuf 0 blockSizeIn outBuf 0 blockSizeOut 

    where

    advanceOutBuf bufOut offsetOut spaceOut count = 
        if count == spaceOut then do
            yield bufOut
            outBuf' <- lift $ mallocForeignBufferAligned blockSizeOut
            return (outBuf', 0, blockSizeOut) 
        else 
            return (bufOut, offsetOut + count, spaceOut - count) 

    simple bufIn offsetIn spaceIn bufOut offsetOut spaceOut = do
        let count = min (spaceIn - numCoeffs + 1) spaceOut
        lift $ single numCoeffs coeffs count offsetIn bufIn offsetOut bufOut

        (bufOut', offsetOut', spaceOut') <- advanceOutBuf bufOut offsetOut spaceOut count

        let spaceIn'  = spaceIn - count
            offsetIn' = offsetIn + count

        case spaceIn' < numCoeffs of
            False -> simple bufIn offsetIn' spaceIn' bufOut' offsetOut' spaceOut'
            True  -> do
                next <- await
                crossover bufIn offsetIn' spaceIn' next bufOut' offsetOut' spaceOut'

    crossover bufLast offsetLast spaceLast bufNext bufOut offsetOut spaceOut = do
        let count = min (spaceLast - 1) spaceOut
        lift $ cross numCoeffs coeffs spaceLast count offsetLast bufLast bufNext offsetOut bufOut

        (bufOut', offsetOut', spaceOut') <- advanceOutBuf bufOut offsetOut spaceOut count

        case spaceLast - 1 == count of 
            True  -> simple bufNext 0 blockSizeIn bufOut' offsetOut' spaceOut'
            False -> crossover bufLast (offsetLast + count) (spaceLast - count) bufNext bufOut' offsetOut' spaceOut'

filterC = filterr filterOneBufC filterCrossBufC
filterR = filterr filterOneBufR filterCrossBufR

--Decimation
type DecimateSingleC a = CInt -> CInt -> Ptr a -> CInt -> Ptr a -> Ptr a -> IO ()
type DecimateCrossC  a = CInt -> CInt -> Ptr a -> CInt -> CInt -> Ptr a -> Ptr a -> Ptr a -> IO ()

foreign import ccall unsafe "decimate_onebuf_c"
    c_decimateOneBufC   :: DecimateSingleC (Complex CDouble)

foreign import ccall unsafe "decimate_crossbuf_c"
    c_decimateCrossBufC :: DecimateCrossC (Complex CDouble)

foreign import ccall unsafe "decimate_onebuf_r"
    c_decimateOneBufR   :: DecimateSingleC CDouble

foreign import ccall unsafe "decimate_crossbuf_r"
    c_decimateCrossBufR :: DecimateCrossC CDouble

type DecimateSingle a = Int -> Int -> ForeignPtr a -> Int -> Int -> ForeignPtr a -> Int -> ForeignPtr a -> IO ()
type DecimateCross  a = Int -> Int -> ForeignPtr a -> Int -> Int -> Int -> ForeignPtr a -> ForeignPtr a -> Int -> ForeignPtr a -> IO ()

decimateOneBuf :: Storable a => DecimateSingleC a -> DecimateSingle a
decimateOneBuf cfunc factor coeffsLength coeffs num inOffset inBuf outOffset outBuf = 
    withForeignPtr coeffs $ \cp -> 
    withForeignPtr inBuf  $ \ip -> 
    withForeignPtr outBuf $ \op -> 
        cfunc (fromIntegral factor) 
              (fromIntegral coeffsLength) 
              cp 
              (fromIntegral num) 
              (advancePtr ip inOffset) 
              (advancePtr op outOffset)

decimateCrossBuf :: Storable a => DecimateCrossC a -> DecimateCross a
decimateCrossBuf cfunc factor coeffsLength coeffs numInput num lastOffset lastBuf nextBuf outOffset outBuf = 
    withForeignPtr coeffs  $ \cp -> 
    withForeignPtr lastBuf $ \lp -> 
    withForeignPtr nextBuf $ \np -> 
    withForeignPtr outBuf  $ \op -> 
        cfunc (fromIntegral factor) 
              (fromIntegral coeffsLength) 
              cp 
              (fromIntegral numInput) 
              (fromIntegral num) 
              (advancePtr lp lastOffset) 
              np 
              (advancePtr op outOffset)

decimateOneBufC   = decimateOneBuf   c_decimateOneBufC
decimateCrossBufC = decimateCrossBuf c_decimateCrossBufC
decimateOneBufR   = decimateOneBuf   c_decimateOneBufR
decimateCrossBufR = decimateCrossBuf c_decimateCrossBufR

decimate :: (Storable a) => DecimateSingle a -> DecimateCross a -> Int -> Int -> ForeignPtr a -> Int -> Int -> Pipe (ForeignPtr a) (ForeignPtr a) IO ()
decimate single cross factor numCoeffs coeffs blockSizeIn blockSizeOut = do
    inBuf  <- await
    outBuf <- lift $ mallocForeignBufferAligned blockSizeOut

    simple inBuf 0 blockSizeIn outBuf 0 blockSizeOut 

    where

    advanceOutBuf bufOut offsetOut spaceOut count = 
        if count == spaceOut then do
            yield bufOut
            outBuf' <- lift $ mallocForeignBufferAligned blockSizeOut
            return (outBuf', 0, blockSizeOut) 
        else 
            return (bufOut, offsetOut + count, spaceOut - count) 

    simple bufIn offsetIn spaceIn bufOut offsetOut spaceOut = do

        assert "" (spaceIn >= numCoeffs) (return ())

        let count = min (((spaceIn - numCoeffs) `quot` factor) + 1) spaceOut
        lift $ single factor numCoeffs coeffs count offsetIn bufIn offsetOut bufOut

        (bufOut', offsetOut', spaceOut') <- advanceOutBuf bufOut offsetOut spaceOut count

        let spaceIn'  = spaceIn - count * factor
            offsetIn' = offsetIn + count * factor

        case spaceIn' < numCoeffs of
            False -> simple bufIn offsetIn' spaceIn' bufOut' offsetOut' spaceOut'
            True  -> do
                next <- await
                crossover bufIn offsetIn' spaceIn' next bufOut' offsetOut' spaceOut'

    crossover bufLast offsetLast spaceLast bufNext bufOut offsetOut spaceOut = do

        assert "" (spaceLast < numCoeffs) (return ())

        let count = min (((spaceLast - 1) `quot` factor) + 1) spaceOut
        lift $ cross factor numCoeffs coeffs spaceLast count offsetLast bufLast bufNext offsetOut bufOut

        (bufOut', offsetOut', spaceOut') <- advanceOutBuf bufOut offsetOut spaceOut count

        case ((spaceLast - 1) `quot` factor) + 1 == count of 
            True  -> simple bufNext (offsetLast + count * factor - blockSizeIn) (blockSizeIn - (offsetLast + count * factor - blockSizeIn)) bufOut' offsetOut' spaceOut'
            False -> crossover bufLast (offsetLast + count * factor) (spaceLast - count * factor) bufNext bufOut' offsetOut' spaceOut'

decimateC = decimate decimateOneBufC decimateCrossBufC
decimateR = decimate decimateOneBufR decimateCrossBufR

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

type ResampleSingle a = Int -> Int -> Int -> ForeignPtr a -> Int -> Int -> Int -> ForeignPtr a -> Int -> ForeignPtr a -> IO Int
type ResampleCross  a = Int -> Int -> Int -> ForeignPtr a -> Int -> Int -> Int -> Int -> ForeignPtr a -> ForeignPtr a -> Int -> ForeignPtr a -> IO Int

resampleOneBuf :: Storable a => ResampleSingleC a -> ResampleSingle a
resampleOneBuf cfunc interpolation decimation coeffsLength coeffs filterOffset count inOffset inBuf outOffset outBuf = liftM fromIntegral $ 
    withForeignPtr coeffs $ \cp -> 
    withForeignPtr inBuf  $ \ip -> 
    withForeignPtr outBuf $ \op -> 
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
    withForeignPtr coeffs  $ \cp -> 
    withForeignPtr lastBuf $ \lp -> 
    withForeignPtr nextBuf $ \np -> 
    withForeignPtr outBuf  $ \op -> 
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

resample :: (Storable a) => ResampleSingle a -> ResampleCross a -> Int -> Int -> Int -> ForeignPtr a -> Int -> Int -> Pipe (ForeignPtr a) (ForeignPtr a) IO ()
resample single cross interpolation decimation numCoeffs coeffs blockSizeIn blockSizeOut = do
    inBuf  <- await
    outBuf <- lift $ mallocForeignBufferAligned blockSizeOut

    simple inBuf 0 blockSizeIn outBuf 0 blockSizeOut 0

    where

    advanceOutBuf bufOut offsetOut spaceOut count = 
        if count == spaceOut then do
            yield bufOut
            outBuf' <- lift $ mallocForeignBufferAligned blockSizeOut
            return (outBuf', 0, blockSizeOut) 
        else 
            return (bufOut, offsetOut + count, spaceOut - count) 

    simple bufIn offsetIn spaceIn bufOut offsetOut spaceOut filterOffset = do

        --filterOffset is the offset in the filter that the first input data is multiplied with

        --Check that we have space in the input buffer for at least one output
        assert "1" (spaceIn * interpolation >= numCoeffs - filterOffset) (return ())
        assert "9" (offsetIn + spaceIn == blockSizeIn) (return ())

        --available number of samples == interpolation * num_input
        --required number of samples  == decimation * (num_output - 1) + filter_length - filter_offset
        let count = min (((spaceIn * interpolation - numCoeffs + filterOffset) `quot` decimation) + 1) spaceOut
        endOffset <- lift $ single interpolation decimation numCoeffs coeffs filterOffset count offsetIn bufIn offsetOut bufOut

        assert "2" ((count * decimation + endOffset - filterOffset) `rem` interpolation == 0) (return ())

        (bufOut', offsetOut', spaceOut') <- advanceOutBuf bufOut offsetOut spaceOut count

        --samples no longer needed starting from filterOffset == count * decimation - filterOffset
        --inputs lying in this region                         == (count * decimation - filterOffset) / interpolation (rounding up)
        let usedInput = (count * decimation - filterOffset) `quotUp` interpolation 
            spaceIn'  = spaceIn  - usedInput
            offsetIn' = offsetIn + usedInput

        case spaceIn' * interpolation < numCoeffs - endOffset of
            False -> do
                simple bufIn offsetIn' spaceIn' bufOut' offsetOut' spaceOut' endOffset
            True  -> do
                next <- await
                case spaceIn' == 0 of
                    True ->  simple next 0 blockSizeIn bufOut' offsetOut' spaceOut' endOffset
                    False -> crossover bufIn offsetIn' spaceIn' next bufOut' offsetOut' spaceOut' endOffset

    crossover bufLast offsetLast spaceLast bufNext bufOut offsetOut spaceOut filterOffset = do

        assert "6" (spaceLast > 0) (return ())
        assert "3" (spaceLast * interpolation < numCoeffs - filterOffset) (return ())

        assert "8" (offsetLast + spaceLast == blockSizeIn) (return ())

        --outputsComputable is the number of outputs that need to be computed for the last buffer to no longer be needed
        --outputsComputable * decimation == numInput * interpolation + filterOffset + k
        let outputsComputable = (spaceLast * interpolation + filterOffset) `quotUp` decimation
            count = min outputsComputable spaceOut
        assert "7" (count /= 0) (return ())

        endOffset <- lift $ cross interpolation decimation numCoeffs coeffs filterOffset spaceLast count offsetLast bufLast bufNext offsetOut bufOut

        assert "2" ((count * decimation + endOffset - filterOffset) `rem` interpolation == 0) (return ())

        (bufOut', offsetOut', spaceOut') <- advanceOutBuf bufOut offsetOut spaceOut count

        let inputUsed = (count * decimation - filterOffset) `quotUp` interpolation

        case inputUsed >= spaceLast of 
            True  -> simple bufNext (offsetLast + inputUsed - blockSizeIn) (2 * blockSizeIn - (offsetLast + inputUsed)) bufOut' offsetOut' spaceOut' endOffset
            False -> crossover bufLast (offsetLast + inputUsed) (spaceLast - inputUsed) bufNext bufOut' offsetOut' spaceOut' endOffset

resampleC = resample resampleOneBufC resampleCrossBufC
resampleR = resample resampleOneBufR resampleCrossBufR

