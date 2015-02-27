{-# LANGUAGE RecordWildCards #-}
module SDR.Filter (
    haskellFilter,
    fastFilterR,
    haskellDecimator,
    fastDecimatorC,
    filterr,
    decimate,
    resample
    ) where

import Foreign.C.Types
import Data.Complex
import Control.Exception 
import qualified Data.Vector.Generic               as VG
import qualified Data.Vector.Generic.Mutable       as VGM
import qualified Data.Vector.Storable              as VS
import qualified Data.Vector.Storable.Mutable      as VSM
import qualified Data.Vector.Fusion.Stream         as VFS
import qualified Data.Vector.Fusion.Stream.Monadic as VFSM
import Control.Monad.Primitive
import Control.Monad

import Pipes

import SDR.Util
import SDR.FilterInternal

data Filter m v vm a = Filter {
    numCoeffsF    :: Int,
    filterOne     :: Int -> v a -> vm (PrimState m) a -> m (),
    filterCross   :: Int -> v a -> v a -> vm (PrimState m) a -> m ()
}

data Decimator m v vm a = Decimator {
    numCoeffsD    :: Int,
    decimateOne   :: Int -> v a -> vm (PrimState m) a -> m (),
    decimateCross :: Int -> v a -> v a -> vm (PrimState m) a -> m ()
}

{-# INLINE haskellFilter #-}
haskellFilter :: (PrimMonad m, Functor m, Num a, Mult a b, VG.Vector v a, VG.Vector v b, VGM.MVector vm a) => [b] -> IO (Filter m v vm a) 
haskellFilter coeffs = do
    let vCoeffs     = VG.fromList coeffs
    evaluate vCoeffs
    let filterOne   = filterHighLevel      vCoeffs
        filterCross = filterCrossHighLevel vCoeffs
        numCoeffsF  = length coeffs
    return $ Filter {..}

{-# INLINE fastFilterR #-}
fastFilterR :: [Float] -> IO (Filter IO VS.Vector VS.MVector Float)
fastFilterR coeffs = do
    let l          = length coeffs
        ru         = (l + 8 - 1) `quot` 8
        numCoeffsF = ru * 8 
        diff       = numCoeffsF - l
        vCoeffs    = VG.fromList $ coeffs ++ replicate diff 0
    evaluate vCoeffs
    let filterOne   = filterCAVXRR         vCoeffs
        filterCross = filterCrossHighLevel vCoeffs
    return $ Filter {..}

{-# INLINE haskellDecimator #-}
haskellDecimator :: (PrimMonad m, Functor m, Num a, Mult a b, VG.Vector v a, VG.Vector v b, VGM.MVector vm a) => Int -> [b] -> IO (Decimator m v vm a)
haskellDecimator factor coeffs = do
    let vCoeffs     = VG.fromList coeffs
    evaluate vCoeffs
    let decimateOne   = decimateHighLevel      factor vCoeffs
        decimateCross = decimateCrossHighLevel factor vCoeffs
        numCoeffsD    = length coeffs
    return $ (Decimator {..})

{-# INLINE fastDecimatorC #-}
fastDecimatorC :: Int -> [Float] -> IO (Decimator IO VS.Vector VS.MVector (Complex Float))
fastDecimatorC factor coeffs = do
    let l          = length coeffs
        ru         = (l + 8 - 1) `quot` 8
        numCoeffsD = ru * 8 
        diff       = numCoeffsD - l
        vCoeffs    = VG.fromList $ coeffs ++ replicate diff 0
    evaluate vCoeffs
    let decimateOne   = decimateCAVXRC         factor vCoeffs
        decimateCross = decimateCrossHighLevel factor vCoeffs
    return $ Decimator {..}

data Buffer v a = Buffer {
    buffer :: v a,
    offset :: Int,
    size   :: Int
}

newBuffer :: (PrimMonad m, VGM.MVector vm a) => Int -> m (Buffer (vm (PrimState m)) a)
newBuffer size = do
    buf <- VGM.new size
    return $ Buffer buf 0 size

advanceOutBuf :: (PrimMonad m, VG.Vector v a) => Int -> Buffer (VG.Mutable v (PrimState m)) a -> Int -> Pipe b (v a) m (Buffer (VG.Mutable v (PrimState m)) a)
advanceOutBuf blockSizeOut (Buffer bufOut offsetOut spaceOut) count = 
    if count == spaceOut then do
        bufOutF <- lift $ VG.unsafeFreeze bufOut
        yield bufOutF
        outBuf' <- lift $ VGM.new blockSizeOut
        return $ Buffer outBuf' 0 blockSizeOut
    else 
        return $ Buffer bufOut (offsetOut + count) (spaceOut - count) 

--Filtering
{-# INLINE filterr #-}
filterr :: (PrimMonad m, Functor m, VG.Vector v a, Num a) => Filter m v (VG.Mutable v) a -> Int -> Int -> Pipe (v a) (v a) m ()
filterr Filter{..} blockSizeIn blockSizeOut = do
    inBuf  <- await
    outBuf <- lift $ newBuffer blockSizeOut
    simple (Buffer inBuf 0 blockSizeIn) outBuf 

    where

    simple (Buffer bufIn offsetIn spaceIn) bufferOut@(Buffer bufOut offsetOut spaceOut) = do
        let count = min (spaceIn - numCoeffsF + 1) spaceOut
        lift $ filterOne count (VG.unsafeDrop offsetIn bufIn) (VGM.unsafeDrop offsetOut bufOut)

        bufferOut' <- advanceOutBuf blockSizeOut bufferOut count

        let spaceIn'  = spaceIn - count
            offsetIn' = offsetIn + count

        case spaceIn' < numCoeffsF of
            False -> simple (Buffer bufIn offsetIn' spaceIn') bufferOut'
            True  -> do
                next <- await
                crossover (Buffer bufIn offsetIn' spaceIn') next bufferOut'

    crossover (Buffer bufLast offsetLast spaceLast) bufNext bufferOut@(Buffer bufOut offsetOut spaceOut) = do
        let count = min (spaceLast - 1) spaceOut
        lift $ filterCross count (VG.unsafeDrop offsetLast bufLast) bufNext (VGM.unsafeDrop offsetOut bufOut)

        bufferOut' <- advanceOutBuf blockSizeOut bufferOut count

        case spaceLast - 1 == count of 
            True  -> simple (Buffer bufNext 0 blockSizeIn) bufferOut'
            False -> crossover (Buffer bufLast (offsetLast + count) (spaceLast - count)) bufNext bufferOut'

--Decimation
{-# INLINE decimate #-}
decimate :: (PrimMonad m, Functor m, VG.Vector v a, Num a) => Decimator m v (VG.Mutable v) a -> Int -> Int -> Int -> Pipe (v a) (v a) m ()
decimate Decimator{..} factor blockSizeIn blockSizeOut = do
    inBuf  <- await
    outBuf <- lift $ newBuffer blockSizeOut
    simple (Buffer inBuf 0 blockSizeIn) outBuf

    where

    simple (Buffer bufIn offsetIn spaceIn) bufferOut@(Buffer bufOut offsetOut spaceOut) = do

        assert (spaceIn >= numCoeffsD) $ return ()

        let count = min (((spaceIn - numCoeffsD) `quot` factor) + 1) spaceOut
        lift $ decimateOne count (VG.unsafeDrop offsetIn bufIn) (VGM.unsafeDrop offsetOut bufOut)

        bufferOut' <- advanceOutBuf blockSizeOut bufferOut count

        let spaceIn'  = spaceIn - count * factor
            offsetIn' = offsetIn + count * factor

        case spaceIn' < numCoeffsD of
            False -> simple (Buffer bufIn offsetIn' spaceIn') bufferOut'
            True  -> do
                next <- await
                crossover (Buffer bufIn offsetIn' spaceIn') next bufferOut'

    crossover (Buffer bufLast offsetLast spaceLast) bufNext bufferOut@(Buffer bufOut offsetOut spaceOut) = do

        assert (spaceLast < numCoeffsD) $ return ()

        let count = min (((spaceLast - 1) `quot` factor) + 1) spaceOut
        lift $ decimateCross count (VG.unsafeDrop offsetLast bufLast) bufNext (VGM.unsafeDrop offsetOut bufOut)

        bufferOut' <- advanceOutBuf blockSizeOut bufferOut count

        case ((spaceLast - 1) `quot` factor) + 1 == count of 
            True  -> simple (Buffer bufNext (offsetLast + count * factor - blockSizeIn) (blockSizeIn - (offsetLast + count * factor - blockSizeIn))) bufferOut'
            False -> crossover (Buffer bufLast (offsetLast + count * factor) (spaceLast - count * factor)) bufNext bufferOut'

{-

Input upsampled by 3:    |**|**|**|**|**|**|**|**|**|**|**|
Output downsampled by 7: |******|******|******|******|*****

                  Consider here ^
                  Next output is here  ^

Filter offset is 2

filterOffset + k*interpolation = decimation + filterOffset'
where
    k > 0
    0 <= filterOffset, filterOffset' < interpolation

k*interpolation - filterOffset' = decimation - filterOffset
k*interpolation - filterOffset' - 1 = decimation - filterOffset - 1

(k-1) * interpolation + (interpolation - filterOffset' - 1) = decimation - filterOffset - 1

k             = (decimation - filterOffset - 1) / interpolation + 1
filterOffset' = interpolation - 1 - (decimation - filterOffset - 1) % interpolation

Only works if decimation > interpolation

-}

--Rational resampling
{-# INLINE resampleOne #-}
resampleOne :: (PrimMonad m, Num a, Mult a b, VG.Vector v a, VG.Vector v b, VGM.MVector vm a) => Int -> Int -> v b -> Int -> Int -> v a -> vm (PrimState m) a -> m Int
resampleOne interpolation decimation coeffs filterOffset count inBuf outBuf = fill 0 filterOffset 0
    where
    fill i filterOffset inputOffset
        | i < count = do
            let dp = dotProd filterOffset inputOffset
            VGM.unsafeWrite outBuf i dp
            let (q, r)        = quotRem (decimation - filterOffset - 1) interpolation
                inputOffset'  = inputOffset + q + 1
                filterOffset' = interpolation - 1 - r
            filterOffset' `seq` inputOffset' `seq` fill (i + 1) filterOffset' inputOffset'
        | otherwise = return filterOffset
    dotProd filterOffset offset = VG.sum $ VG.zipWith mult (VG.unsafeDrop offset inBuf) (stride interpolation (VG.unsafeDrop filterOffset coeffs))

{-# INLINE resampleCross #-}
resampleCross :: (PrimMonad m, Num a, Mult a b, VG.Vector v a, VG.Vector v b, VGM.MVector vm a) => Int -> Int -> v b -> Int -> Int -> v a -> v a -> vm (PrimState m) a -> m Int
resampleCross interpolation decimation coeffs filterOffset count lastBuf nextBuf outBuf = fill 0 filterOffset 0
    where
    fill i filterOffset inputOffset
        | i < count = do
            let dp = dotProd filterOffset inputOffset
            VGM.unsafeWrite outBuf i dp
            let (q, r)        = quotRem (decimation - filterOffset - 1) interpolation
                inputOffset'  = inputOffset + q + 1
                filterOffset' = interpolation - 1 - r
            filterOffset' `seq` inputOffset' `seq` fill (i + 1) filterOffset' inputOffset'
        | otherwise = return filterOffset
    dotProd filterOffset i = VG.sum $ VG.zipWith mult (VG.unsafeDrop i lastBuf VG.++ nextBuf) (stride interpolation (VG.unsafeDrop filterOffset coeffs))

quotUp q d = (q + (d - 1)) `quot` d

{-# INLINE resample #-}
resample :: (PrimMonad m, VG.Vector v a, VG.Vector v b, Mult a b, Num a) => Int -> Int -> v b -> Int -> Int -> Pipe (v a) (v a) m ()
resample interpolation decimation coeffs blockSizeIn blockSizeOut = resample' (VG.length coeffs) coeffs
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
            endOffset <- lift $ resampleOne interpolation decimation coeffs filterOffset count (VG.unsafeDrop offsetIn bufIn) (VGM.unsafeDrop offsetOut bufOut)
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
            endOffset <- lift $ resampleCross interpolation decimation coeffs filterOffset count (VG.unsafeDrop offsetLast bufLast) bufNext (VGM.unsafeDrop offsetOut bufOut)
            --Check consistency
            assert ((count * decimation + endOffset - filterOffset) `rem` interpolation == 0) $ return ()
            --Advance the output buffer
            bufferOut' <- advanceOutBuf blockSizeOut bufferOut count

            let inputUsed = (count * decimation - filterOffset) `quotUp` interpolation

            case inputUsed >= spaceLast of 
                True  -> simple (Buffer bufNext (offsetLast + inputUsed - blockSizeIn) (2 * blockSizeIn - (offsetLast + inputUsed))) bufferOut' endOffset
                False -> crossover (Buffer bufLast (offsetLast + inputUsed) (spaceLast - inputUsed)) bufNext bufferOut' endOffset

