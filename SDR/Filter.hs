{-# LANGUAGE RecordWildCards #-}
module SDR.Filter (
    haskellFilter,
    fastFilterR,
    fastFilterC,
    fastSymmetricFilterR,
    haskellDecimator,
    fastDecimatorR,
    fastDecimatorC,
    fastSymmetricDecimatorR,
    haskellResampler,
    filterr,
    decimate,
    resample
    ) where

import           Foreign.C.Types
import           Data.Complex
import           Control.Exception 
import qualified Data.Vector.Generic               as VG
import qualified Data.Vector.Generic.Mutable       as VGM
import qualified Data.Vector.Storable              as VS
import qualified Data.Vector.Storable.Mutable      as VSM
import qualified Data.Vector.Fusion.Stream         as VFS
import qualified Data.Vector.Fusion.Stream.Monadic as VFSM
import           Control.Monad.Primitive
import           Control.Monad

import           Pipes

import           SDR.Util
import           SDR.FilterInternal

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

data Resampler m v vm a = Resampler {
    numCoeffsR    :: Int,
    resampleOne   :: Int -> Int -> v a -> vm (PrimState m) a -> m Int,
    resampleCross :: Int -> Int -> v a -> v a -> vm (PrimState m) a -> m Int
}

duplicate :: [a] -> [a]
duplicate = concat . map func 
    where func x = [x, x]

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

{-# INLINE fastFilterC #-}
fastFilterC :: [Float] -> IO (Filter IO VS.Vector VS.MVector (Complex Float))
fastFilterC coeffs = do
    let l           = length coeffs
        ru          = (l + 8 - 1) `quot` 8
        numCoeffsF  = ru * 8 
        diff        = numCoeffsF - l
        vCoeffs     = VG.fromList $ duplicate $ coeffs ++ replicate diff 0
    evaluate vCoeffs
    let filterOne   = filterCAVXRC         vCoeffs
        filterCross = filterCrossHighLevel vCoeffs
    return $ Filter {..}

{-# INLINE fastSymmetricFilterR #-}
-- | Coefficient length must be a multiple of 4
fastSymmetricFilterR :: [Float] -> IO (Filter IO VS.Vector VS.MVector Float)
fastSymmetricFilterR coeffs = do
    let vCoeffs     = VG.fromList coeffs 
    let vCoeffs2    = VG.fromList $ coeffs ++ reverse coeffs
    evaluate vCoeffs
    evaluate vCoeffs2
    let filterOne   = filterCAVXSymmetricRR vCoeffs
        filterCross = filterCrossHighLevel  vCoeffs2
        numCoeffsF  = length coeffs * 2
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

{-# INLINE fastDecimatorR #-}
fastDecimatorR :: Int -> [Float] -> IO (Decimator IO VS.Vector VS.MVector Float)
fastDecimatorR factor coeffs = do
    let l          = length coeffs
        ru         = (l + 8 - 1) `quot` 8
        numCoeffsD = ru * 8 
        diff       = numCoeffsD - l
        vCoeffs    = VG.fromList $ coeffs ++ replicate diff 0
    evaluate vCoeffs
    let decimateOne   = decimateCAVXRR         factor vCoeffs
        decimateCross = decimateCrossHighLevel factor vCoeffs
    return $ Decimator {..}

{-# INLINE fastDecimatorC #-}
fastDecimatorC :: Int -> [Float] -> IO (Decimator IO VS.Vector VS.MVector (Complex Float))
fastDecimatorC factor coeffs = do
    let l          = length coeffs
        ru         = (l + 8 - 1) `quot` 8
        numCoeffsD = ru * 8 
        diff       = numCoeffsD - l
        vCoeffs    = VG.fromList $ duplicate $ coeffs ++ replicate diff 0
    evaluate vCoeffs
    let decimateOne   = decimateCAVXRC         factor vCoeffs
        decimateCross = decimateCrossHighLevel factor vCoeffs
    return $ Decimator {..}

{-# INLINE fastSymmetricDecimatorR #-}
fastSymmetricDecimatorR :: Int -> [Float] -> IO (Decimator IO VS.Vector VS.MVector Float)
fastSymmetricDecimatorR factor coeffs = do
    let vCoeffs    = VG.fromList coeffs
    let vCoeffs2   = VG.fromList $ coeffs ++ reverse coeffs
    evaluate vCoeffs
    evaluate vCoeffs2
    let decimateOne   = decimateCAVXSymmetricRR factor vCoeffs
        decimateCross = decimateCrossHighLevel  factor vCoeffs2
        numCoeffsD    = length coeffs * 2
    return $ Decimator {..}

{-# INLINE haskellResampler #-}
haskellResampler :: (PrimMonad m, Functor m, Num a, Mult a b, VG.Vector v a, VG.Vector v b, VGM.MVector vm a) => Int -> Int -> [b] -> IO (Resampler m v vm a) 
haskellResampler interpolation decimation coeffs = do
    let vCoeffs     = VG.fromList coeffs
    evaluate vCoeffs
    let resampleOne   = resampleHighLevel      interpolation decimation vCoeffs
        resampleCross = resampleCrossHighLevel interpolation decimation vCoeffs
        numCoeffsR  = length coeffs
    return $ Resampler {..}

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
quotUp q d = (q + (d - 1)) `quot` d

{-# INLINE resample #-}
resample :: (PrimMonad m, VG.Vector v a, Num a) => Resampler m v (VG.Mutable v) a -> Int -> Int -> Int -> Int -> Pipe (v a) (v a) m ()
resample Resampler{..} interpolation decimation blockSizeIn blockSizeOut = do
    inBuf  <- await
    outBuf <- lift $ newBuffer blockSizeOut
    simple (Buffer inBuf 0 blockSizeIn) outBuf 0

    where

    simple (Buffer bufIn offsetIn spaceIn) bufferOut@(Buffer bufOut offsetOut spaceOut) filterOffset = do
        --Check consistency
        assert (spaceIn * interpolation >= numCoeffsR - filterOffset) $ return ()
        assert (offsetIn + spaceIn == blockSizeIn) $ return ()
        --available number of samples == interpolation * num_input
        --required number of samples  == decimation * (num_output - 1) + filter_length - filter_offset
        let count = min (((spaceIn * interpolation - numCoeffsR + filterOffset) `quot` decimation) + 1) spaceOut
        --Run filter
        endOffset <- lift $ resampleOne filterOffset count (VG.unsafeDrop offsetIn bufIn) (VGM.unsafeDrop offsetOut bufOut)
        --Check consistency
        assert ((count * decimation + endOffset - filterOffset) `rem` interpolation == 0) $ return ()
        --Advance the output buffer
        bufferOut' <- advanceOutBuf blockSizeOut bufferOut count
        --samples no longer needed starting from filterOffset == count * decimation - filterOffset
        --inputs lying in this region                         == (count * decimation - filterOffset) / interpolation (rounding up)
        let usedInput = (count * decimation - filterOffset) `quotUp` interpolation 
            spaceIn'  = spaceIn  - usedInput
            offsetIn' = offsetIn + usedInput

        case spaceIn' * interpolation < numCoeffsR - endOffset of
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
        assert (spaceLast * interpolation < numCoeffsR - filterOffset) $ return ()
        assert (offsetLast + spaceLast == blockSizeIn) $ return ()
        --outputsComputable is the number of outputs that need to be computed for the last buffer to no longer be needed
        --outputsComputable * decimation == numInput * interpolation + filterOffset + k
        let outputsComputable = (spaceLast * interpolation + filterOffset) `quotUp` decimation
            count = min outputsComputable spaceOut
        assert (count /= 0) $ return ()
        --Run the filter
        endOffset <- lift $ resampleCross filterOffset count (VG.unsafeDrop offsetLast bufLast) bufNext (VGM.unsafeDrop offsetOut bufOut)
        --Check consistency
        assert ((count * decimation + endOffset - filterOffset) `rem` interpolation == 0) $ return ()
        --Advance the output buffer
        bufferOut' <- advanceOutBuf blockSizeOut bufferOut count

        let inputUsed = (count * decimation - filterOffset) `quotUp` interpolation

        case inputUsed >= spaceLast of 
            True  -> simple (Buffer bufNext (offsetLast + inputUsed - blockSizeIn) (2 * blockSizeIn - (offsetLast + inputUsed))) bufferOut' endOffset
            False -> crossover (Buffer bufLast (offsetLast + inputUsed) (spaceLast - inputUsed)) bufNext bufferOut' endOffset

