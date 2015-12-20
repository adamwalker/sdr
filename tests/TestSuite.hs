{-# LANGUAGE ScopedTypeVariables #-}

import           Control.Monad.Primitive 
import           Data.Complex
import           Control.Monad

import qualified Data.Vector.Generic               as VG
import qualified Data.Vector.Generic.Mutable       as VGM
import qualified Data.Vector.Storable              as VS
import qualified Data.Vector.Storable.Mutable      as VSM

import           Test.QuickCheck
import           Test.QuickCheck.Monadic
import           Test.Framework (defaultMain, testGroup)
import           Test.Framework.Providers.QuickCheck2 (testProperty)

import           SDR.FilterInternal
import           SDR.Util
import           SDR.CPUID

sameResultM :: Monad m => (a -> a -> Bool) -> [m a] -> m Bool
sameResultM _  []     = return True
sameResultM eq (x:xs) = do
    res  <- x
    ress <- sequence xs
    return $ and $ map (eq res) ress

sameResult :: (a -> a -> Bool) -> [a] -> Bool
sameResult _ [] = True
sameResult eq (x:xs) = and $ map (eq x) xs

tests info = [
        testGroup "filters" [
            testProperty "real"    propFiltersReal,
            testProperty "complex" propFiltersComplex
        ],
        testGroup "decimators" [
            testProperty "real"    propDecimationReal,
            testProperty "complex" propDecimationComplex
        ],
        testGroup "resamplers" [
            testProperty "real" propResamplingReal,
            testProperty "complex" propResamplingComplex
        ],
        testGroup "conversion" [
            testProperty "rtlsdr"  propConversionRTLSDR,
            testProperty "BladeRF" propConversionBladeRF
        ],
        testProperty "scaling"    propScaleReal
    ]
    where
    hasFeatures :: [(CPUInfo -> Bool, a)] -> [a]
    hasFeatures = map snd . filter (($ info) . fst)

    sizes           = elements [1024, 2048, 4096, 8192, 16384, 32768, 65536]
    numCoeffs       = elements [32, 64, 128, 256, 512]
    factors'        = [1, 2, 3, 5, 7, 11, 13, 17, 23]
    factors         = elements factors'

    propFiltersReal = 
        forAll sizes $ \size -> 
            forAll (vectorOf size (choose (-10, 10))) $ \inBuf -> 
                forAll numCoeffs $ \numCoeffs -> 
                    forAll (vectorOf numCoeffs (choose (-10, 10))) $ \coeffs -> 
                        testFiltersReal size numCoeffs coeffs inBuf

    testFiltersReal :: Int -> Int -> [Float] -> [Float] -> Property
    testFiltersReal size numCoeffs coeffs inBuf = monadicIO $ do
        let vCoeffsHalf = VS.fromList coeffs
            vCoeffs     = VS.fromList $ coeffs ++ reverse coeffs
            vInput      = VS.fromList inBuf
            num         = size - numCoeffs*2 + 1

        res <- run $ sameResultM eqDelta $ map (getResult num $) $ hasFeatures [
                (const True, filterHighLevel       vCoeffs     num vInput),
                (const True, filterImperative1     vCoeffs     num vInput),
                (const True, filterImperative2     vCoeffs     num vInput),
                (const True, filterCRR             vCoeffs     num vInput),
                (hasSSE42,   filterCSSERR          vCoeffs     num vInput),
                (hasAVX,     filterCAVXRR          vCoeffs     num vInput),
                (hasSSE42,   filterCSSESymmetricRR vCoeffsHalf num vInput),
                (hasAVX,     filterCAVXSymmetricRR vCoeffsHalf num vInput)
            ]
        assert res

    propFiltersComplex = 
        forAll sizes $ \size -> 
            forAll (vectorOf size (choose (-10, 10))) $ \inBufR -> 
                forAll (vectorOf size (choose (-10, 10))) $ \inBufI -> 
                    forAll numCoeffs $ \numCoeffs -> 
                        forAll (vectorOf numCoeffs (choose (-10, 10))) $ \coeffs -> 
                            testFiltersComplex size numCoeffs coeffs $ zipWith (:+) inBufR inBufI

    testFiltersComplex :: Int -> Int -> [Float] -> [Complex Float] -> Property
    testFiltersComplex size numCoeffs coeffs inBuf = monadicIO $ do
        let vCoeffsHalf = VS.fromList coeffs
            vCoeffs     = VS.fromList $ coeffs ++ reverse coeffs
            vInput      = VS.fromList inBuf
            num         = size - numCoeffs*2 + 1
            vCoeffs2    = VG.fromList $ duplicate $ coeffs ++ reverse coeffs

        res <- run $ sameResultM eqDeltaC $ map (getResult num $) $ hasFeatures [
                (const True, filterHighLevel       vCoeffs  num vInput),
                (const True, filterCRC             vCoeffs  num vInput),
                (hasSSE42,   filterCSSERC          vCoeffs2 num vInput),
                (hasSSE42,   filterCSSERC2         vCoeffs  num vInput),
                (hasAVX,     filterCAVXRC          vCoeffs2 num vInput),
                (hasSSE42,   filterCSSESymmetricRC vCoeffsHalf num vInput),
                (hasAVX,     filterCAVXSymmetricRC vCoeffsHalf num vInput)
            ]
        assert res

    propDecimationReal = 
        forAll sizes $ \size -> 
            forAll (vectorOf size (choose (-10, 10))) $ \inBuf -> 
                forAll numCoeffs $ \numCoeffs -> 
                    forAll (vectorOf numCoeffs (choose (-10, 10))) $ \coeffs -> 
                       forAll factors $ \factor -> 
                            testDecimationReal size numCoeffs factor coeffs inBuf

    testDecimationReal :: Int -> Int -> Int -> [Float] -> [Float] -> Property
    testDecimationReal size numCoeffs factor coeffs inBuf = monadicIO $ do
        let vCoeffsHalf = VS.fromList coeffs
            vCoeffs     = VS.fromList $ coeffs ++ reverse coeffs
            vInput      = VS.fromList inBuf
            num         = (size - numCoeffs*2 + 1) `quot` factor

        res <- run $ sameResultM eqDelta $ map (getResult num $) $ hasFeatures [
                (const True, decimateHighLevel       factor vCoeffs     num vInput),
                (const True, decimateCRR             factor vCoeffs     num vInput),
                (hasSSE42,   decimateCSSERR          factor vCoeffs     num vInput),
                (hasAVX,     decimateCAVXRR          factor vCoeffs     num vInput),
                (hasSSE42,   decimateCSSESymmetricRR factor vCoeffsHalf num vInput),
                (hasAVX,     decimateCAVXSymmetricRR factor vCoeffsHalf num vInput)
            ]
        assert res

    propDecimationComplex = 
        forAll sizes $ \size -> 
            forAll (vectorOf size (choose (-10, 10))) $ \inBufR -> 
                forAll (vectorOf size (choose (-10, 10))) $ \inBufI -> 
                    forAll numCoeffs $ \numCoeffs -> 
                        forAll (vectorOf numCoeffs (choose (-10, 10))) $ \coeffs -> 
                            forAll factors $ \factor -> 
                                testDecimationComplex size numCoeffs factor coeffs $ zipWith (:+) inBufR inBufI

    testDecimationComplex :: Int -> Int -> Int -> [Float] -> [Complex Float] -> Property
    testDecimationComplex size numCoeffs factor coeffs inBuf = monadicIO $ do
        let vCoeffsHalf = VS.fromList coeffs
            vCoeffs     = VS.fromList $ coeffs ++ reverse coeffs
            vInput      = VS.fromList inBuf
            num         = (size - numCoeffs*2 + 1) `quot` factor
            vCoeffs2    = VG.fromList $ duplicate $ coeffs ++ reverse coeffs

        res <- run $ sameResultM eqDeltaC $ map (getResult num $) $ hasFeatures [
                (const True, decimateHighLevel       factor vCoeffs  num vInput),
                (const True, decimateCRC             factor vCoeffs  num vInput),
                (hasSSE42,   decimateCSSERC          factor vCoeffs2 num vInput),
                (hasSSE42,   decimateCSSERC2         factor vCoeffs  num vInput),
                (hasAVX,     decimateCAVXRC          factor vCoeffs2 num vInput),
                (hasSSE42,   decimateCSSESymmetricRC factor vCoeffsHalf  num vInput),
                (hasAVX,     decimateCAVXRC2         factor vCoeffs  num vInput),
                (hasAVX,     decimateCAVXSymmetricRC factor vCoeffsHalf  num vInput)
            ]
        assert res

    propResamplingReal = 
        forAll sizes $ \size -> 
            forAll (vectorOf size (choose (-10, 10))) $ \inBuf -> 
                forAll (elements [32 .. 512]) $ \numCoeffs -> 
                    forAll (vectorOf numCoeffs (choose (-10, 10))) $ \coeffs -> 
                       forAll (elements $ tail factors') $ \decimation -> 
                           forAll (elements $ filter (< decimation) factors') $ \interpolation -> 
                               forAll (elements [0..interpolation - 1]) $ \group -> 
                                   testResamplingReal size group numCoeffs interpolation decimation coeffs inBuf

    testResamplingReal :: Int -> Int -> Int -> Int -> Int -> [Float] -> [Float] -> Property
    testResamplingReal size group numCoeffs interpolation decimation coeffs inBuf = monadicIO $ do
        let vCoeffsHalf = VS.fromList coeffs
            vCoeffs     = VS.fromList $ coeffs ++ reverse coeffs
            vInput      = VS.fromList inBuf
            num         = (size - numCoeffs*2 + 1) `quot` decimation
            offset       = interpolation - 1 - ((interpolation + group * decimation - 1) `mod` interpolation) 

        resampler3 <- run $ resampleCRR2   interpolation decimation (coeffs ++ reverse coeffs)
        resampler4 <- run $ resampleCSSERR interpolation decimation (coeffs ++ reverse coeffs)
        resampler5 <- run $ resampleCAVXRR interpolation decimation (coeffs ++ reverse coeffs)

        res <- run $ sameResultM eqDelta $ map (getResult num $) $ hasFeatures [
                (const True, void . resampleHighLevel       interpolation decimation vCoeffs offset num vInput),
                (const True, void . resampleCRR             num interpolation decimation offset vCoeffs vInput),
                (const True, void . resampler3              group num vInput),
                (hasSSE42,   void . resampler4              group num vInput),
                (hasAVX,     void . resampler5              group num vInput)
            ]
        assert res

    propResamplingComplex = 
        forAll sizes $ \size -> 
            forAll (vectorOf size (choose (-10, 10))) $ \inBufR -> 
                forAll (vectorOf size (choose (-10, 10))) $ \inBufI -> 
                    forAll (elements [32 .. 512]) $ \numCoeffs -> 
                        forAll (vectorOf numCoeffs (choose (-10, 10))) $ \coeffs -> 
                           forAll (elements $ tail factors') $ \decimation -> 
                               forAll (elements $ filter (< decimation) factors') $ \interpolation -> 
                                   forAll (elements [0..interpolation - 1]) $ \group -> 
                                       testResamplingComplex size group numCoeffs interpolation decimation coeffs $ zipWith (:+) inBufR inBufI

    testResamplingComplex :: Int -> Int -> Int -> Int -> Int -> [Float] -> [Complex Float] -> Property
    testResamplingComplex size group numCoeffs interpolation decimation coeffs inBuf = monadicIO $ do
        let vCoeffsHalf = VS.fromList coeffs
            vCoeffs     = VS.fromList $ coeffs ++ reverse coeffs
            vInput      = VS.fromList inBuf
            num         = (size - numCoeffs*2 + 1) `quot` decimation
            offset       = interpolation - 1 - ((interpolation + group * decimation - 1) `mod` interpolation) 

        resampler3 <- run $ resampleCRC    interpolation decimation (coeffs ++ reverse coeffs)
        resampler4 <- run $ resampleCSSERC interpolation decimation (coeffs ++ reverse coeffs)
        resampler5 <- run $ resampleCAVXRC interpolation decimation (coeffs ++ reverse coeffs)

        res <- run $ sameResultM eqDeltaC $ map (getResult num $) $ hasFeatures [
                (const True, void . resampleHighLevel       interpolation decimation vCoeffs offset num vInput),
                (const True, void . resampler3              group num vInput),
                (hasSSE42,   void . resampler4              group num vInput),
                (hasAVX,     void . resampler5              group num vInput)
            ]
        assert res

    propConversionRTLSDR = 
        forAll sizes $ \size -> 
            forAll (vectorOf (2 * size) (choose (-10, 10))) $ \inBuf -> 
                testConversionRTLSDR size inBuf
    testConversionRTLSDR :: Int -> [Int] -> Property
    testConversionRTLSDR size inBuf = monadicIO $ do
        let vInput = VS.fromList $ map fromIntegral inBuf

        let res = sameResult eqDeltaC $ map (VG.toList $) $ hasFeatures [
                (const True, (interleavedIQUnsigned256ToFloat    vInput :: VS.Vector (Complex Float))),
                (const True, interleavedIQUnsignedByteToFloat    vInput),
                (hasSSE42,   interleavedIQUnsignedByteToFloatSSE vInput),
                (hasAVX2,    interleavedIQUnsignedByteToFloatAVX vInput)
                ]
        assert res

    propConversionBladeRF = 
        forAll sizes $ \size -> 
            forAll (vectorOf (2 * size) (choose (-10, 10))) $ \inBuf -> 
                testConversionBladeRF size inBuf
    testConversionBladeRF :: Int -> [Int] -> Property
    testConversionBladeRF size inBuf = monadicIO $ do
        let vInput = VS.fromList $ map fromIntegral inBuf

        let res = sameResult eqDeltaC $ map (VG.toList $) $ hasFeatures [
                (const True, (interleavedIQSigned2048ToFloat   vInput :: VS.Vector (Complex Float))),
                (const True, interleavedIQSignedWordToFloat    vInput),
                (hasSSE42,   interleavedIQSignedWordToFloatSSE vInput),
                (hasAVX2,    interleavedIQSignedWordToFloatAVX vInput)
                ]
        assert res

    scales        = elements [0.1, 0.5, 1, 2, 10]
    propScaleReal = 
        forAll sizes $ \size -> 
            forAll (vectorOf size (choose (-10, 10))) $ \inBuf -> 
                forAll scales $ \factor -> 
                    testScaleReal size inBuf factor
    testScaleReal :: Int -> [Float] -> Float -> Property
    testScaleReal size inBuf factor = monadicIO $ do
        let vInput = VS.fromList inBuf

        res <- run $ sameResultM eqDelta $ map (getResult size $) $ hasFeatures [
                (const True, scaleC    factor vInput),
                (hasSSE42,   scaleCSSE factor vInput),
                (hasAVX,     scaleCAVX factor vInput)
            ]
        assert res

    getResult :: (VSM.Storable a) => Int -> (VS.MVector RealWorld a -> IO b) -> IO [a]
    getResult size func = do
        outBuf <- VGM.new size
        func outBuf
        out :: VS.Vector a <- VG.freeze outBuf
        return $ VG.toList out
    eqDelta x y = and $ map (uncurry eqDelta') $ zip x y
        where
        eqDelta' x y = abs (x - y) < 0.01
    eqDeltaC x y = and $ map (uncurry eqDelta') $ zip x y
        where
        eqDelta' x y = magnitude (x - y) < 0.01
    duplicate :: [a] -> [a]
    duplicate = concatMap func
        where func x = [x, x]

main = do
    info <- getCPUInfo 
    defaultMain $ tests info

