{-# LANGUAGE RecordWildCards #-}

{-| FIR filtering, decimation and resampling.

    FIR filters (and decimators, resamplers) work by taking successive dot products between the filter coefficients and the input data at increasing offsets. Sometimes the dot product fits entirely within one input buffer and other times it spans two input buffers (but never more because we assume that the filter length is less than the buffer size).

    We divide the filtering code by these two cases. Each filter (or decimator, resampler) is described by a data structure such as `Filter` with two functions, one for filtering within a single buffer and one that crosses buffers. 

    The user must first create one of these data structures using the helper functions and pass this data structure to one of `filterr`, `decimate`, or `resample` to create the `Pipe` that does the filtering. For example:

    > decimatorStruct   <- fastDecimatorC decimation coeffs
    > let decimatorPipe :: Pipe (Vector (Complex Float)) (Vector (Complex Float)) IO ()
    >     decimatorPipe =  decimate decimatorStruct outputSize

    There are polymorphic Haskell only implementations of filtering, decimation and resampling, for example, `haskellFilter`. In addition, there are optimised C implementations that use SIMD instructions on x86 machines, such as `fastFilterR`. These are always specialized to either real or complex numbers. There are also even faster implementations specialized for the case where the filter coefficients are symmetric as in a linear phase filter such as `fastSymmetricFilterR`.

    The Haskell implementations are reasonably fast due to the Vector library and GHC's LLVM backend, however, if speed is important you are much better off with the C implementations.

    In the future we may avoid the cross buffer filtering function by mapping the buffers consecutively in memory as (I believe) GNU Radio does.

    An extensive benchmark suite exists in the /benchmarks subdirectory of this package.

    TODO: interpolation not implemented.
-}
module SDR.Filter (
    -- * Types
    Filter(..),
    Decimator(..),
    Resampler(..),

    -- * Helper Functions
    -- ** Filters
    haskellFilter,
    fastFilterR,
    fastFilterC,
    fastSymmetricFilterR,

    -- ** Decimators
    haskellDecimator,
    fastDecimatorR,
    fastDecimatorC,
    fastSymmetricDecimatorR,

    -- ** Resamplers
    haskellResampler,
    fastResampler,

    -- * Filter
    filterr,

    -- * Decimate
    decimate,

    -- * Resample
    resample,

    -- * DC Blocking Filter
    dcBlockingFilter
    ) where

import           Data.Complex
import           Control.Exception           hiding (assert)
import qualified Data.Vector.Generic         as VG
import qualified Data.Vector.Generic.Mutable as VGM
import qualified Data.Vector.Storable        as VS
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
    decimationD   :: Int,
    decimateOne   :: Int -> v a -> vm (PrimState m) a -> m (),
    decimateCross :: Int -> v a -> v a -> vm (PrimState m) a -> m ()
}

{- | A `Resampler` contains all of the information needed by the `resample` 
     function to perform resampling i.e. it contains the filter coefficients 
     and pointers to the functions to do the actual resampling.
-}
data Resampler m v vm a = Resampler {
    numCoeffsR     :: Int,
    decimationR    :: Int,
    interpolationR :: Int,
    resampleOne    :: Int -> Int -> v a -> vm (PrimState m) a -> m Int,
    resampleCross  :: Int -> Int -> v a -> v a -> vm (PrimState m) a -> m Int
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
        ru          = (l + 4 - 1) `quot` 4
        numCoeffsF  = ru * 4 
        diff        = numCoeffsF - l
        vCoeffs     = VG.fromList $ duplicate $ coeffs ++ replicate diff 0
        vCoeffs2    = VG.fromList $ coeffs ++ replicate diff 0
    evaluate vCoeffs
    let filterOne   = filterCAVXRC         vCoeffs
        filterCross = filterCrossHighLevel vCoeffs2
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
haskellDecimator decimationD coeffs = do
    let vCoeffs     = VG.fromList coeffs
    evaluate vCoeffs
    let decimateOne   = decimateHighLevel      decimationD vCoeffs
        decimateCross = decimateCrossHighLevel decimationD vCoeffs
        numCoeffsD    = length coeffs
    return $ Decimator {..}

{-# INLINE fastDecimatorR #-}
-- | Returns a fast Decimator data structure implemented in C using AVX instructions. For decimating real data with real coefficients.
fastDecimatorR :: Int                                          -- ^ The decimation factor
               -> [Float]                                      -- ^ The filter coefficients
               -> IO (Decimator IO VS.Vector VS.MVector Float) -- ^ The `Decimator` data structure
fastDecimatorR decimationD coeffs = do
    let l          = length coeffs
        ru         = (l + 8 - 1) `quot` 8
        numCoeffsD = ru * 8 
        diff       = numCoeffsD - l
        vCoeffs    = VG.fromList $ coeffs ++ replicate diff 0
    evaluate vCoeffs
    let decimateOne   = decimateCAVXRR         decimationD vCoeffs
        decimateCross = decimateCrossHighLevel decimationD vCoeffs
    return $ Decimator {..}

{-# INLINE fastDecimatorC #-}
-- | Returns a fast Decimator data structure implemented in C using AVX instructions. For decimating complex data with real coefficients.
fastDecimatorC :: Int                                                    -- ^ The decimation factor
               -> [Float]                                                -- ^ The filter coefficients
               -> IO (Decimator IO VS.Vector VS.MVector (Complex Float)) -- ^ The `Decimator` data structure
fastDecimatorC decimationD coeffs = do
    let l          = length coeffs
        ru         = (l + 4 - 1) `quot` 4
        numCoeffsD = ru * 4 
        diff       = numCoeffsD - l
        vCoeffs    = VG.fromList $ duplicate $ coeffs ++ replicate diff 0
        vCoeffs2   = VG.fromList $ coeffs ++ replicate diff 0
    evaluate vCoeffs
    let decimateOne   = decimateCAVXRC         decimationD vCoeffs
        decimateCross = decimateCrossHighLevel decimationD vCoeffs2
    return $ Decimator {..}

{-# INLINE fastSymmetricDecimatorR #-}
-- | Returns a fast Decimator data structure implemented in C using AVX instructions. For decimating real data with real coefficients. For decimators with symmetric coefficients, i.e. 'linear phase'. Coefficient length must be a multiple of 4.
fastSymmetricDecimatorR :: Int                                          -- ^ The decimation factor
                        -> [Float]                                      -- ^ The first half of the filter coefficients
                        -> IO (Decimator IO VS.Vector VS.MVector Float) -- ^ The `Decimator` data structure
fastSymmetricDecimatorR decimationD coeffs = do
    let vCoeffs    = VG.fromList coeffs
    let vCoeffs2   = VG.fromList $ coeffs ++ reverse coeffs
    evaluate vCoeffs
    evaluate vCoeffs2
    let decimateOne   = decimateCAVXSymmetricRR decimationD vCoeffs
        decimateCross = decimateCrossHighLevel  decimationD vCoeffs2
        numCoeffsD    = length coeffs * 2
    return $ Decimator {..}

{-# INLINE haskellResampler #-}
-- | Returns a slow Resampler data structure entirely implemented in Haskell
haskellResampler :: (PrimMonad m, Functor m, Num a, Mult a b, VG.Vector v a, VG.Vector v b, VGM.MVector vm a) 
                  => Int                     -- ^ The interpolation factor
                  -> Int                     -- ^ The decimation factor
                  -> [b]                     -- ^ The filter coefficients
                  -> IO (Resampler m v vm a) -- ^ The `Resampler` data structure
haskellResampler interpolationR decimationR coeffs = do
    let vCoeffs     = VG.fromList coeffs
    evaluate vCoeffs
    let resampleOne   = resampleHighLevel      interpolationR decimationR vCoeffs
        resampleCross = resampleCrossHighLevel interpolationR decimationR vCoeffs
        numCoeffsR  = length coeffs
    return $ Resampler {..}

{-# INLINE fastResampler #-}
-- | Returns a fast Resampler data structure implemented in C using AVX instructions. For filtering real data with real coefficients.
fastResampler :: Int                                          -- ^ The interpolation factor
              -> Int                                          -- ^ The decimation factor
              -> [Float]                                      -- ^ The filter coefficients
              -> IO (Resampler IO VS.Vector VS.MVector Float) -- ^ The `Resampler` data structure
fastResampler interpolationR decimationR coeffs = do
    let vCoeffs     = VG.fromList coeffs
    evaluate vCoeffs
    resamp <- resampleCAVXRR interpolationR decimationR coeffs
    let resampleOne   = resamp
        resampleCross = resampleCrossHighLevel interpolationR decimationR vCoeffs
        numCoeffsR    = length coeffs
    return $ Resampler {..}

data Buffer v a = Buffer {
    buffer :: v a,
    offset :: Int
}

space Buffer{..} = VGM.length buffer - offset

newBuffer :: (PrimMonad m, VGM.MVector vm a) => Int -> m (Buffer (vm (PrimState m)) a)
newBuffer size = do
    buf <- VGM.new size
    return $ Buffer buf 0

advanceOutBuf :: (PrimMonad m, VG.Vector v a) => Int -> Buffer (VG.Mutable v (PrimState m)) a -> Int -> Pipe b (v a) m (Buffer (VG.Mutable v (PrimState m)) a)
advanceOutBuf blockSizeOut buf@(Buffer bufOut offsetOut) count = 
    if count == space buf then do
        bufOutF <- lift $ VG.unsafeFreeze bufOut
        yield bufOutF
        lift $ newBuffer blockSizeOut
    else 
        return $ Buffer bufOut (offsetOut + count) 

-- | My own assert implementation since the GHC one doesnt seem to work even with optimisations disabled and using -fno-ignore-asserts
assert loc False = error loc
assert loc True  = return ()

--Filtering
{-# INLINE filterr #-}
{-| Create a pipe that performs filtering -}
filterr :: (PrimMonad m, Functor m, VG.Vector v a, Num a) 
        => Filter m v (VG.Mutable v) a -- ^ The `Filter` data structure
        -> Int                         -- ^ The output block size
        -> Pipe (v a) (v a) m ()       -- ^ The `Pipe` that does the filtering
filterr Filter{..} blockSizeOut = do
    inBuf  <- await
    outBuf <- lift $ newBuffer blockSizeOut
    simple inBuf outBuf 

    where

    simple bufIn bufferOut@(Buffer bufOut offsetOut) = do
        assert "filter 1" (VG.length bufIn >= numCoeffsF)

        let count = min (VG.length bufIn - numCoeffsF + 1) (space bufferOut)
        lift $ filterOne count bufIn (VGM.unsafeDrop offsetOut bufOut)

        bufferOut' <- advanceOutBuf blockSizeOut bufferOut count
        let bufIn' =  VG.drop count bufIn

        case VG.length bufIn' < numCoeffsF of
            False -> simple bufIn' bufferOut'
            True  -> do
                next <- await
                crossover bufIn' next bufferOut'

    crossover bufLast bufNext bufferOut@(Buffer bufOut offsetOut) = do
        assert "filter 2" (VG.length bufLast < numCoeffsF) 
        assert "filter 3" (VG.length bufLast > 0) 

        let count = min (VG.length bufLast) (space bufferOut)
        lift $ filterCross count bufLast bufNext (VGM.unsafeDrop offsetOut bufOut)

        bufferOut' <- advanceOutBuf blockSizeOut bufferOut count

        case VG.length bufLast == count of 
            True  -> simple bufNext bufferOut'
            False -> crossover (VG.drop count bufLast) bufNext bufferOut'

--Decimation
{-# INLINE decimate #-}
{-| Create a pipe that performs decimation -}
decimate :: (PrimMonad m, Functor m, VG.Vector v a, Num a) 
         => Decimator m v (VG.Mutable v) a -- ^ The `Decimator` data structure
         -> Int                            -- ^ The output block size
         -> Pipe (v a) (v a) m ()          -- ^ The `Pipe` that does the decimation
decimate Decimator{..} blockSizeOut = do
    inBuf  <- await
    outBuf <- lift $ newBuffer blockSizeOut
    simple inBuf outBuf

    where

    simple bufIn bufferOut@(Buffer bufOut offsetOut) = do
        assert "decimate 1" (VG.length bufIn >= numCoeffsD)

        let count = min (((VG.length bufIn - numCoeffsD) `quot` decimationD) + 1) (space bufferOut)
        lift $ decimateOne count bufIn (VGM.unsafeDrop offsetOut bufOut)

        bufferOut' <- advanceOutBuf blockSizeOut bufferOut count
        let bufIn' = VG.drop (count * decimationD) bufIn

        case VG.length bufIn' < numCoeffsD of
            False -> simple bufIn' bufferOut'
            True  -> do
                next <- await
                crossover bufIn' next bufferOut'

    crossover bufLast bufNext bufferOut@(Buffer bufOut offsetOut) = do
        assert "decimate 2" (VG.length bufLast < numCoeffsD) 
        assert "decimate 3" (VG.length bufLast > 0) 

        let count = min (VG.length bufLast `quotUp` decimationD) (space bufferOut)
        lift $ decimateCross count bufLast bufNext (VGM.unsafeDrop offsetOut bufOut)

        bufferOut' <- advanceOutBuf blockSizeOut bufferOut count

        case VG.length bufLast <= count * decimationD of 
            True  -> simple (VG.drop (count * decimationD - VG.length bufLast) bufNext) bufferOut'
            False -> crossover (VG.drop (count * decimationD) bufLast) bufNext bufferOut'

{-
Rational Downsampling:

Input upsampled by 3:    |**|**|**|**|**|**|**|**|**|**|**|
Output downsampled by 7: |******|******|******|******|*****

                  Consider here ^
                  Next output is here  ^

Filter offset is 2

k is number of used inputs

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

{-
Rational Upsampling:

Input upsampled by 7:    |******|******|******|******|*****
Output downsampled by 3: |**|**|**|**|**|**|**|**|**|**|**|

              Consider Here ^  
              Next sample is   ^

Filter offset is 4

filterOffset + k * interpolation = decimation + filterOffset'
where
    k = {0, 1}
    0 <= filterOffset, filterOffset' < interpolation

k * interpolation + (interpolation - filterOffset' - 1) = decimation - filterOffset + interpolation - 1

k = (decimation - filterOffset + interpolation - 1) / interpolation

============================

Or, equivalently, 

k = 0 | filterOffset >= decimation
    1 | otherwise

o = o - decimation + k * interpolation

-}

--Rational resampling
quotUp q d = (q + (d - 1)) `quot` d

{-# INLINE resample #-}
{-| Create a pipe that performs resampling -}
resample :: (PrimMonad m, VG.Vector v a, Num a) 
         => Resampler m v (VG.Mutable v) a -- ^ The `Resampler` data structure
         -> Int                            -- ^ The output block size
         -> Pipe (v a) (v a) m ()          -- ^ The `Pipe` that does the resampling
resample Resampler{..} blockSizeOut = do
    inBuf  <- await
    outBuf <- lift $ newBuffer blockSizeOut
    simple inBuf outBuf 0

    where

    simple bufIn bufferOut@(Buffer bufOut offsetOut) filterOffset = do
        assert "resample 1" (VG.length bufIn * interpolationR >= numCoeffsR - filterOffset)
        --available number of samples == interpolation * num_input
        --required number of samples  == decimation * (num_output - 1) + filter_length - filter_offset
        let count = min (((VG.length bufIn * interpolationR - numCoeffsR + filterOffset) `quot` decimationR) + 1) (space bufferOut)
        endOffset <- lift $ resampleOne filterOffset count bufIn (VGM.unsafeDrop offsetOut bufOut)
        assert "resample 2" ((count * decimationR + endOffset - filterOffset) `rem` interpolationR == 0)
        bufferOut' <- advanceOutBuf blockSizeOut bufferOut count
        --samples no longer needed starting from filterOffset == count * decimation - filterOffset
        --inputs lying in this region                         == (count * decimation - filterOffset) / interpolation (rounding up)
        let usedInput = (count * decimationR - filterOffset) `quotUp` interpolationR 
            bufIn'    = VG.drop usedInput bufIn

        case VG.length bufIn' * interpolationR < numCoeffsR - endOffset of
            False -> simple bufIn' bufferOut' endOffset
            True  -> do
                next <- await
                --TODO: why is this not needed in filter and decimator
                case VG.length bufIn' == 0 of
                    True ->  simple    next bufferOut' endOffset
                    False -> crossover bufIn' next bufferOut' endOffset

    crossover bufLast bufNext bufferOut@(Buffer bufOut offsetOut) filterOffset = do
        assert "resample 3" (VG.length bufLast * interpolationR < numCoeffsR - filterOffset)
        --outputsComputable is the number of outputs that need to be computed for the last buffer to no longer be needed
        --outputsComputable * decimation == numInput * interpolation + filterOffset + k
        let outputsComputable = (VG.length bufLast * interpolationR + filterOffset) `quotUp` decimationR
            count = min outputsComputable (space bufferOut)
        assert "resample 4" (count /= 0)
        endOffset <- lift $ resampleCross filterOffset count bufLast bufNext (VGM.unsafeDrop offsetOut bufOut)
        assert "resample 5" ((count * decimationR + endOffset - filterOffset) `rem` interpolationR == 0)
        bufferOut' <- advanceOutBuf blockSizeOut bufferOut count

        let inputUsed = (count * decimationR - filterOffset) `quotUp` interpolationR

        case inputUsed >= VG.length bufLast of 
            True  -> simple (VG.drop (inputUsed - VG.length bufLast) bufNext) bufferOut' endOffset
            False -> crossover (VG.drop inputUsed bufLast) bufNext bufferOut' endOffset

-- | A DC blocking filter
dcBlockingFilter :: Pipe (VS.Vector Float) (VS.Vector Float) IO ()
dcBlockingFilter = func 0 0 
    where
    func lastSample lastOutput = do
        dat <- await
        out <- lift $ VGM.new (VG.length dat)
        (lastSample, lastOutput) <- lift $ dcBlocker (VG.length dat) lastSample lastOutput dat out
        outF <- lift $ VG.unsafeFreeze out
        yield outF
        func lastSample lastOutput

