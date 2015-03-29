{-# LANGUAGE RecordWildCards #-}

{-| Filtering, decimation and resampling -}
module SDR.Filter (
    Filter(..),
    Decimator(..),
    Resampler(..),
    haskellFilter,
    fastFilterR,
    fastFilterC,
    fastSymmetricFilterR,
    haskellDecimator,
    fastDecimatorR,
    fastDecimatorC,
    fastSymmetricDecimatorR,
    haskellResampler,
    fastResampler,
    filterr,
    decimate,
    resample
    ) where

import           Data.Complex
import           Control.Exception 
import qualified Data.Vector.Generic               as VG
import qualified Data.Vector.Generic.Mutable       as VGM
import qualified Data.Vector.Storable              as VS
import           Control.Monad.Primitive

import           Pipes

import           SDR.Util
import           SDR.FilterInternal

{- | A `Filter` contains all of the information needed by the `filterr` 
     function to perform filtering. i.e. it contains the filter coefficients 
     and pointers to the functions to do the actual filtering.
-}
data Filter m v vm a = Filter {
    numCoeffsF    :: Int,
    filterOne     :: Int -> v a -> vm (PrimState m) a -> m (),
    filterCross   :: Int -> v a -> v a -> vm (PrimState m) a -> m ()
}

{- | A `Decimator` contains all of the information needed by the `decimate`
     function to perform decimation i.e. it contains the filter coefficients 
     and pointers to the functions to do the actual decimation.
-}
data Decimator m v vm a = Decimator {
    numCoeffsD    :: Int,
    decimateOne   :: Int -> v a -> vm (PrimState m) a -> m (),
    decimateCross :: Int -> v a -> v a -> vm (PrimState m) a -> m ()
}

{- | A `Resampler` contains all of the information needed by the `resample` 
     function to perform resampling i.e. it contains the filter coefficients 
     and pointers to the functions to do the actual resampling.
-}
data Resampler m v vm a = Resampler {
    numCoeffsR    :: Int,
    resampleOne   :: Int -> Int -> v a -> vm (PrimState m) a -> m Int,
    resampleCross :: Int -> Int -> v a -> v a -> vm (PrimState m) a -> m Int
}

duplicate :: [a] -> [a]
duplicate = concat . map func 
    where func x = [x, x]

{-# INLINE haskellFilter #-}
-- | Returns a slow Filter data structure entirely implemented in Haskell
haskellFilter :: (PrimMonad m, Functor m, Num a, Mult a b, VG.Vector v a, VG.Vector v b, VGM.MVector vm a) 
              => [b]                  -- ^ The filter coefficients
              -> IO (Filter m v vm a) -- ^ The `Filter` data structure
haskellFilter coeffs = do
    let vCoeffs     = VG.fromList coeffs
    evaluate vCoeffs
    let filterOne   = filterHighLevel      vCoeffs
        filterCross = filterCrossHighLevel vCoeffs
        numCoeffsF  = length coeffs
    return $ Filter {..}

{-# INLINE fastFilterR #-}
-- | Returns a fast Filter data structure implemented in C using AVX instructions. For filtering real data with real coefficients.
fastFilterR :: [Float]                                   -- ^ The filter coefficients
            -> IO (Filter IO VS.Vector VS.MVector Float) -- ^ The `Filter` data structure
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
-- | Returns a fast Filter data structure implemented in C using AVX instructions. For filtering complex data with real coefficients.
fastFilterC :: [Float]                                             -- ^ The filter coefficients
            -> IO (Filter IO VS.Vector VS.MVector (Complex Float)) -- ^ The `Filter` data structure
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
-- | Returns a fast Filter data structure implemented in C using AVX instructions. For filtering real data with real coefficients. For filters with symmetric coefficients, i.e. 'linear phase'. Coefficient length must be a multiple of 4.
fastSymmetricFilterR :: [Float]                                   -- ^ The first half of the filter coefficients
                     -> IO (Filter IO VS.Vector VS.MVector Float) -- ^ The `Filter` data structure
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
-- | Returns a slow Decimator data structure entirely implemented in Haskell
haskellDecimator :: (PrimMonad m, Functor m, Num a, Mult a b, VG.Vector v a, VG.Vector v b, VGM.MVector vm a) 
                 => Int                     -- ^ The decimation factor
                 -> [b]                     -- ^ The filter coefficients
                 -> IO (Decimator m v vm a) -- ^ The `Decimator` data structure
haskellDecimator factor coeffs = do
    let vCoeffs     = VG.fromList coeffs
    evaluate vCoeffs
    let decimateOne   = decimateHighLevel      factor vCoeffs
        decimateCross = decimateCrossHighLevel factor vCoeffs
        numCoeffsD    = length coeffs
    return $ (Decimator {..})

{-# INLINE fastDecimatorR #-}
-- | Returns a fast Decimator data structure implemented in C using AVX instructions. For decimating real data with real coefficients.
fastDecimatorR :: Int                                          -- ^ The decimation factor
               -> [Float]                                      -- ^ The filter coefficients
               -> IO (Decimator IO VS.Vector VS.MVector Float) -- ^ The `Decimator` data structure
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
-- | Returns a fast Decimator data structure implemented in C using AVX instructions. For decimating complex data with real coefficients.
fastDecimatorC :: Int                                                    -- ^ The decimation factor
               -> [Float]                                                -- ^ The filter coefficients
               -> IO (Decimator IO VS.Vector VS.MVector (Complex Float)) -- ^ The `Decimator` data structure
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
-- | Returns a fast Decimator data structure implemented in C using AVX instructions. For decimating real data with real coefficients. For decimators with symmetric coefficients, i.e. 'linear phase'. Coefficient length must be a multiple of 4.
fastSymmetricDecimatorR :: Int                                          -- ^ The decimation factor
                        -> [Float]                                      -- ^ The first half of the filter coefficients
                        -> IO (Decimator IO VS.Vector VS.MVector Float) -- ^ The `Decimator` data structure
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
-- | Returns a slow Decimator data structure entirely implemented in Haskell
haskellResampler :: (PrimMonad m, Functor m, Num a, Mult a b, VG.Vector v a, VG.Vector v b, VGM.MVector vm a) 
                  => Int                     -- ^ The interpolation factor
                  -> Int                     -- ^ The decimation factor
                  -> [b]                     -- ^ The filter coefficients
                  -> IO (Resampler m v vm a) -- ^ The `Resampler` data structure
haskellResampler interpolation decimation coeffs = do
    let vCoeffs     = VG.fromList coeffs
    evaluate vCoeffs
    let resampleOne   = resampleHighLevel      interpolation decimation vCoeffs
        resampleCross = resampleCrossHighLevel interpolation decimation vCoeffs
        numCoeffsR  = length coeffs
    return $ Resampler {..}

{-# INLINE fastResampler #-}
-- | Returns a fast Filter data structure implemented in C using AVX instructions. For filtering real data with real coefficients.
fastResampler :: Int                                          -- ^ The interpolation factor
              -> Int                                          -- ^ The decimation factor
              -> [Float]                                      -- ^ The filter coefficients
              -> IO (Resampler IO VS.Vector VS.MVector Float) -- ^ The `Resampler` data structure
fastResampler interpolation decimation coeffs = do
    let vCoeffs     = VG.fromList coeffs
    evaluate vCoeffs
    resamp <- resampleCAVXRR interpolation decimation coeffs
    let resampleOne   = resamp
        resampleCross = resampleCrossHighLevel interpolation decimation vCoeffs
        numCoeffsR    = length coeffs
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
{-| Create a pipe that performs filtering -}
filterr :: (PrimMonad m, Functor m, VG.Vector v a, Num a) 
        => Filter m v (VG.Mutable v) a -- ^ The `Filter` data structure
        -> Int                         -- ^ The input block size
        -> Int                         -- ^ The output block size
        -> Pipe (v a) (v a) m ()       -- ^ The `Pipe` that does the filtering
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
{-| Create a pipe that performs decimation -}
decimate :: (PrimMonad m, Functor m, VG.Vector v a, Num a) 
         => Decimator m v (VG.Mutable v) a -- ^ The `Decimator` data structure
         -> Int                            -- ^ The decimation factor
         -> Int                            -- ^ The input block size
         -> Int                            -- ^ The output block size
         -> Pipe (v a) (v a) m ()          -- ^ The `Pipe` that does the decimation
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
{-| Create a pipe that performs resampling -}
resample :: (PrimMonad m, VG.Vector v a, Num a) 
         => Resampler m v (VG.Mutable v) a -- ^ The `Resampler` data structure
         -> Int                            -- ^ The interpolation factor
         -> Int                            -- ^ The decimation factor
         -> Int                            -- ^ The input block size
         -> Int                            -- ^ The output block size
         -> Pipe (v a) (v a) m ()          -- ^ The `Pipe` that does the resampling
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

