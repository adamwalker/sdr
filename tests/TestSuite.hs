{-# LANGUAGE ScopedTypeVariables #-}

import           System.Exit
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

import           Test.QuickCheck
import           Test.QuickCheck.Monadic

import           Foreign.Storable.Complex

import           SDR.FilterInternal

theTest = quickCheck $ conjoin [counterexample "Real Filters" propFiltersReal, counterexample "Complex Filters" propFiltersComplex, counterexample "Real Decimators" propDecimationReal, counterexample "Complex Decimators" propDecimationComplex]
    where
    sizes           = elements [1024, 2048, 4096, 8192, 16384, 32768, 65536]
    numCoeffs       = elements [32, 64, 128, 256, 512]
    factors         = elements [1, 2, 3, 4, 7, 9, 12, 15, 21]
    propFiltersReal = forAll sizes $ \size -> 
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

        r1 <- run $ getResult num $ filterHighLevel       num vCoeffs     vInput
        r2 <- run $ getResult num $ filterImperative1     num vCoeffs     vInput
        r3 <- run $ getResult num $ filterImperative2     num vCoeffs     vInput
        r4 <- run $ getResult num $ filterCRR             num vCoeffs     vInput
        r5 <- run $ getResult num $ filterCSSERR          num vCoeffs     vInput
        r6 <- run $ getResult num $ filterCAVXRR          num vCoeffs     vInput
        --r7 <- run $ getResult num $ filterCSSESymmetricRR num vCoeffsHalf vInput
        --r8 <- run $ getResult num $ filterCAVXSymmetricRR num vCoeffsHalf vInput

        assert $ and $ map (r1 `eqDelta`) [r2, r3, r4, r5, r6{-, r7, r8-}]
    propFiltersComplex = forAll sizes $ \size -> 
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

        r1 <- run $ getResult num $ filterHighLevel       num vCoeffs     vInput
        r2 <- run $ getResult num $ filterCRC             num vCoeffs     vInput
        r3 <- run $ getResult num $ filterCSSERC          num vCoeffs2    vInput
        r4 <- run $ getResult num $ filterCSSERC2         num vCoeffs     vInput
        r5 <- run $ getResult num $ filterCAVXRC          num vCoeffs2    vInput

        assert $ and $ map (r1 `eqDeltaC`) [r2, r3, r4, r5]
    propDecimationReal = forAll sizes $ \size -> 
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

        r1 <- run $ getResult num $ decimateHighLevel       num factor vCoeffs     vInput
        r2 <- run $ getResult num $ decimateCRR             num factor vCoeffs     vInput
        r3 <- run $ getResult num $ decimateCSSERR          num factor vCoeffs     vInput
        r4 <- run $ getResult num $ decimateCAVXRR          num factor vCoeffs     vInput
        --r5 <- run $ getResult num $ decimateCSSESymmetricRR num factor vCoeffsHalf vInput
        --r6 <- run $ getResult num $ decimateCAVXSymmetricRR num factor vCoeffsHalf vInput

        assert $ and $ map (r1 `eqDelta`) [r2, r3, r4{-, r5, r6-}]
    propDecimationComplex = forAll sizes $ \size -> 
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

        r1 <- run $ getResult num $ decimateHighLevel       num factor vCoeffs     vInput
        r2 <- run $ getResult num $ decimateCRC             num factor vCoeffs     vInput
        r3 <- run $ getResult num $ decimateCSSERC          num factor vCoeffs2    vInput
        r4 <- run $ getResult num $ decimateCSSERC2         num factor vCoeffs     vInput
        r5 <- run $ getResult num $ decimateCAVXRC          num factor vCoeffs2    vInput

        assert $ and $ map (r1 `eqDeltaC`) [r2, r3, r4, r5]
    getResult :: (VSM.Storable a) => Int -> (VS.MVector RealWorld a -> IO ()) -> IO [a]
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
    duplicate = concat . map func 
        where func x = [x, x]

main = theTest >> exitSuccess

