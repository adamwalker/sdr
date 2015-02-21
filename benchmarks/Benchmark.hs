{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances, ScopedTypeVariables, BangPatterns #-}

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

import           Foreign.Storable.Complex
import           Criterion.Main
import           Test.QuickCheck
import           Test.QuickCheck.Monadic

-- | A class for things that can be multiplied by a scalar.
class Mult a b where
    mult :: a -> b -> a

instance (Num a) => Mult a a where
    mult = (*)

instance (Num a) => Mult (Complex a) a where
    mult (x :+ y) z = (x * z) :+ (y * z)

-- | Fill a mutable vector from a monadic stream
{-# INLINE fill #-}
fill :: (PrimMonad m, Functor m, VGM.MVector vm a) => VFS.MStream m a -> vm (PrimState m) a -> m ()
fill str outBuf = void $ VFSM.foldM' put 0 str
    where 
    put i x = do
        VGM.unsafeWrite outBuf i x
        return $ i + 1
       
{-# INLINE stride #-}
stride :: VG.Vector v a => Int -> v a -> v a
stride str inv = VG.unstream $ VFS.unfoldr func 0
    where
    len = VG.length inv
    func i | i >= len  = Nothing
           | otherwise = Just (VG.unsafeIndex inv i, i + str)

-- | The functions to be benchmarked

-- | Filters

filterHighLevel :: (PrimMonad m, Functor m, Num a, Mult a b, VG.Vector v a, VG.Vector v b, VGM.MVector vm a) => Int -> v b -> v a -> vm (PrimState m) a -> m ()
filterHighLevel num coeffs inBuf outBuf = fill (VFSM.generate num dotProd) outBuf
    where
    dotProd offset = VG.sum $ VG.zipWith mult (VG.unsafeDrop offset inBuf) coeffs

filterImperative1 :: (PrimMonad m, Functor m, Num a, Mult a b, VG.Vector v a, VG.Vector v b, VGM.MVector vm a) => Int -> v b -> v a -> vm (PrimState m) a -> m ()
filterImperative1 num coeffs inBuf outBuf = go 0
    where
    go offset 
        | offset < num = do
            let res = dotProd offset
            VGM.unsafeWrite outBuf offset res
            go $ offset + 1
        | otherwise    = return ()
    dotProd offset = VG.sum $ VG.zipWith mult (VG.unsafeDrop offset inBuf) coeffs

filterImperative2 :: (PrimMonad m, Functor m, Num a, Mult a b, VG.Vector v a, VG.Vector v b, VGM.MVector vm a) => Int -> v b -> v a -> vm (PrimState m) a -> m ()
filterImperative2 num coeffs inBuf outBuf = go 0
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

type FilterCRR = CInt -> CInt -> Ptr CFloat -> Ptr CFloat -> Ptr CFloat -> IO ()
type FilterRR  = Int -> VS.Vector Float -> VS.Vector Float -> VS.MVector RealWorld Float -> IO ()
type FilterRC  = Int -> VS.Vector Float -> VS.Vector (Complex Float) -> VS.MVector RealWorld (Complex Float) -> IO ()

filterFFIR :: FilterCRR -> FilterRR 
filterFFIR func num coeffs inBuf outBuf = 
    VS.unsafeWith (unsafeCoerce coeffs) $ \cPtr -> 
        VS.unsafeWith (unsafeCoerce inBuf) $ \iPtr -> 
            VSM.unsafeWith (unsafeCoerce outBuf) $ \oPtr -> 
                func (fromIntegral num) (fromIntegral $ VG.length coeffs) cPtr iPtr oPtr

filterFFIC :: FilterCRR -> FilterRC 
filterFFIC func num coeffs inBuf outBuf = 
    VS.unsafeWith (unsafeCoerce coeffs) $ \cPtr -> 
        VS.unsafeWith (unsafeCoerce inBuf) $ \iPtr -> 
            VSM.unsafeWith (unsafeCoerce outBuf) $ \oPtr -> 
                func (fromIntegral num) (fromIntegral $ VG.length coeffs) cPtr iPtr oPtr

foreign import ccall unsafe "filterRR"
    filterRR_c :: CInt -> CInt -> Ptr CFloat -> Ptr CFloat -> Ptr CFloat -> IO ()

filterCRR :: FilterRR
filterCRR = filterFFIR filterRR_c 

foreign import ccall unsafe "filterRC"
    filterRC_c :: CInt -> CInt -> Ptr CFloat -> Ptr CFloat -> Ptr CFloat -> IO ()

filterCRC :: FilterRC
filterCRC = filterFFIC filterRC_c

foreign import ccall unsafe "filterSSERR"
    filterSSERR_c :: CInt -> CInt -> Ptr CFloat -> Ptr CFloat -> Ptr CFloat -> IO ()

filterCSSERR :: FilterRR
filterCSSERR = filterFFIR filterSSERR_c

foreign import ccall unsafe "filterSSESymmetricRR"
    filterSSESymmetricRR_c :: CInt -> CInt -> Ptr CFloat -> Ptr CFloat -> Ptr CFloat -> IO ()

filterCSSESymmetricRR :: FilterRR
filterCSSESymmetricRR = filterFFIR filterSSESymmetricRR_c

foreign import ccall unsafe "filterSSERC"
    filterSSERC_c :: CInt -> CInt -> Ptr CFloat -> Ptr CFloat -> Ptr CFloat -> IO ()

filterCSSERC :: FilterRC
filterCSSERC = filterFFIC filterSSERC_c

foreign import ccall unsafe "filterSSERC2"
    filterSSERC2_c :: CInt -> CInt -> Ptr CFloat -> Ptr CFloat -> Ptr CFloat -> IO ()

filterCSSERC2 :: FilterRC
filterCSSERC2 = filterFFIC filterSSERC2_c

foreign import ccall unsafe "filterSSESymmetricRC"
    filterSSESymmetricRC_c :: CInt -> CInt -> Ptr CFloat -> Ptr CFloat -> Ptr CFloat -> IO ()

filterCSSESymmetricRC :: FilterRC
filterCSSESymmetricRC = filterFFIC filterSSESymmetricRC_c

foreign import ccall unsafe "filterAVXRR"
    filterAVXRR_c :: CInt -> CInt -> Ptr CFloat -> Ptr CFloat -> Ptr CFloat -> IO ()

filterCAVXRR :: FilterRR
filterCAVXRR = filterFFIR filterAVXRR_c

foreign import ccall unsafe "filterAVXSymmetricRR"
    filterAVXSymmetricRR_c :: CInt -> CInt -> Ptr CFloat -> Ptr CFloat -> Ptr CFloat -> IO ()

filterCAVXSymmetricRR :: FilterRR
filterCAVXSymmetricRR = filterFFIR filterAVXSymmetricRR_c

foreign import ccall unsafe "filterAVXRC"
    filterAVXRC_c :: CInt -> CInt -> Ptr CFloat -> Ptr CFloat -> Ptr CFloat -> IO ()

filterCAVXRC :: FilterRC
filterCAVXRC = filterFFIC filterAVXRC_c

-- | Decimation

decimateHighLevel :: (PrimMonad m, Functor m, Num a, Mult a b, VG.Vector v a, VG.Vector v b, VGM.MVector vm a) => Int -> Int -> v b -> v a -> vm (PrimState m) a -> m ()
decimateHighLevel num factor coeffs inBuf outBuf = fill x outBuf
    where 
    x = VFSM.map dotProd (VFSM.iterateN num (+ factor) 0)
    dotProd offset = VG.sum $ VG.zipWith mult (VG.unsafeDrop offset inBuf) coeffs

type DecimateCRR = CInt -> CInt -> CInt -> Ptr CFloat -> Ptr CFloat -> Ptr CFloat -> IO ()
type DecimateRR  = Int -> Int -> VS.Vector Float -> VS.Vector Float -> VS.MVector RealWorld Float -> IO ()
type DecimateRC  = Int -> Int -> VS.Vector Float -> VS.Vector (Complex Float) -> VS.MVector RealWorld (Complex Float) -> IO ()

decimateFFIR :: DecimateCRR -> DecimateRR 
decimateFFIR func num factor coeffs inBuf outBuf = 
    VS.unsafeWith (unsafeCoerce coeffs) $ \cPtr -> 
        VS.unsafeWith (unsafeCoerce inBuf) $ \iPtr -> 
            VSM.unsafeWith (unsafeCoerce outBuf) $ \oPtr -> 
                func (fromIntegral num) (fromIntegral factor) (fromIntegral $ VG.length coeffs) cPtr iPtr oPtr

decimateFFIC :: DecimateCRR -> DecimateRC 
decimateFFIC func num factor coeffs inBuf outBuf = 
    VS.unsafeWith (unsafeCoerce coeffs) $ \cPtr -> 
        VS.unsafeWith (unsafeCoerce inBuf) $ \iPtr -> 
            VSM.unsafeWith (unsafeCoerce outBuf) $ \oPtr -> 
                func (fromIntegral num) (fromIntegral factor) (fromIntegral $ VG.length coeffs) cPtr iPtr oPtr

foreign import ccall unsafe "decimateRR"
    decimateCRR_c :: CInt -> CInt -> CInt -> Ptr CFloat -> Ptr CFloat -> Ptr CFloat -> IO ()

decimateCRR :: DecimateRR
decimateCRR = decimateFFIR decimateCRR_c

foreign import ccall unsafe "decimateRC"
    decimateCRC_c :: CInt -> CInt -> CInt -> Ptr CFloat -> Ptr CFloat -> Ptr CFloat -> IO ()

decimateCRC :: DecimateRC
decimateCRC = decimateFFIC decimateCRC_c

foreign import ccall unsafe "decimateSSERR"
    decimateSSERR_c :: CInt -> CInt -> CInt -> Ptr CFloat -> Ptr CFloat -> Ptr CFloat -> IO ()

decimateCSSERR :: DecimateRR
decimateCSSERR = decimateFFIR decimateSSERR_c

foreign import ccall unsafe "decimateSSERC"
    decimateSSERC_c :: CInt -> CInt -> CInt -> Ptr CFloat -> Ptr CFloat -> Ptr CFloat -> IO ()

decimateCSSERC :: DecimateRC
decimateCSSERC = decimateFFIC decimateSSERC_c

foreign import ccall unsafe "decimateSSERC2"
    decimateSSERC2_c :: CInt -> CInt -> CInt -> Ptr CFloat -> Ptr CFloat -> Ptr CFloat -> IO ()

decimateCSSERC2 :: DecimateRC
decimateCSSERC2 = decimateFFIC decimateSSERC2_c

foreign import ccall unsafe "decimateAVXRR"
    decimateAVXRR_c :: CInt -> CInt -> CInt -> Ptr CFloat -> Ptr CFloat -> Ptr CFloat -> IO ()

decimateCAVXRR :: DecimateRR
decimateCAVXRR = decimateFFIR decimateAVXRR_c

foreign import ccall unsafe "decimateAVXRC"
    decimateAVXRC_c :: CInt -> CInt -> CInt -> Ptr CFloat -> Ptr CFloat -> Ptr CFloat -> IO ()

decimateCAVXRC :: DecimateRC
decimateCAVXRC = decimateFFIC decimateAVXRC_c

foreign import ccall unsafe "decimateSSESymmetricRR"
    decimateSSESymmetricRR_c :: CInt -> CInt -> CInt -> Ptr CFloat -> Ptr CFloat -> Ptr CFloat -> IO ()

decimateCSSESymmetricRR :: DecimateRR
decimateCSSESymmetricRR = decimateFFIR decimateSSESymmetricRR_c

foreign import ccall unsafe "decimateAVXSymmetricRR"
    decimateAVXSymmetricRR_c :: CInt -> CInt -> CInt -> Ptr CFloat -> Ptr CFloat -> Ptr CFloat -> IO ()

decimateCAVXSymmetricRR :: DecimateRR
decimateCAVXSymmetricRR = decimateFFIR decimateAVXSymmetricRR_c

-- | Rational downsampling
resampleHighLevel :: (PrimMonad m, Num a, Mult a b, VG.Vector v a, VG.Vector v b, VGM.MVector vm a) => Int -> Int -> Int -> Int -> v b -> v a -> vm (PrimState m) a -> m Int
resampleHighLevel count interpolation decimation filterOffset coeffs inBuf outBuf = fill 0 filterOffset 0
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

foreign import ccall unsafe "resample"
    resample_c :: CInt -> CInt -> CInt -> CInt -> CInt -> Ptr CFloat -> Ptr CFloat -> Ptr CFloat -> IO ()

resampleC :: Int -> Int -> Int -> Int -> VS.Vector Float -> VS.Vector Float -> VS.MVector RealWorld Float -> IO ()
resampleC num interpolation decimation offset coeffs inBuf outBuf = 
    VS.unsafeWith (unsafeCoerce coeffs) $ \cPtr -> 
        VS.unsafeWith (unsafeCoerce inBuf) $ \iPtr -> 
            VS.unsafeWith (unsafeCoerce outBuf) $ \oPtr -> 
                resample_c (fromIntegral num) (fromIntegral $ VG.length coeffs) (fromIntegral interpolation) (fromIntegral decimation) (fromIntegral offset) cPtr iPtr oPtr

-- | Conversion

foreign import ccall unsafe "convertC"
    convertC_c :: CInt -> Ptr CUChar -> Ptr CFloat -> IO ()

convertC :: Int -> VS.Vector CUChar -> VS.MVector RealWorld Float -> IO ()
convertC num inBuf outBuf = 
    VS.unsafeWith inBuf $ \iPtr -> 
        VSM.unsafeWith (unsafeCoerce outBuf) $ \oPtr -> 
            convertC_c (fromIntegral num) iPtr oPtr

convertHighLevel :: VS.Vector CUChar -> VS.Vector Float 
convertHighLevel = VG.map fromIntegral

-- | Scaling
foreign import ccall unsafe "scale"
    scale_c :: CInt -> CFloat -> Ptr CFloat -> IO ()

scaleC :: Int -> Float -> VS.MVector RealWorld Float -> IO ()
scaleC num factor buf = 
    VS.unsafeWith (unsafeCoerce buf) $ \bPtr -> 
        scale_c (fromIntegral num) (unsafeCoerce factor) bPtr

foreign import ccall unsafe "scaleSSE"
    scaleSSE_c :: CInt -> CFloat -> Ptr CFloat -> IO ()

scaleCSSE :: Int -> Float -> VS.MVector RealWorld Float -> IO ()
scaleCSSE num factor buf = 
    VS.unsafeWith (unsafeCoerce buf) $ \bPtr -> 
        scaleSSE_c (fromIntegral num) (unsafeCoerce factor) bPtr

foreign import ccall unsafe "scaleAVX"
    scaleAVX_c :: CInt -> CFloat -> Ptr CFloat -> IO ()

scaleCAVX :: Int -> Float -> VS.MVector RealWorld Float -> IO ()
scaleCAVX num factor buf = 
    VS.unsafeWith (unsafeCoerce buf) $ \bPtr -> 
        scaleAVX_c (fromIntegral num) (unsafeCoerce factor) bPtr

theBench :: IO ()
theBench = do
    --Setup
    let size       =  16384
        numCoeffs  =  128
        num        =  size - numCoeffs + 1
        decimation =  4
        interpolation = 3
        numCoeffsDiv2  =  64

        coeffs    :: VS.Vector Float
        coeffs    =  VG.fromList $ take numCoeffs [0 ..]
        coeffsSym    :: VS.Vector Float
        coeffsSym    =  VG.fromList $ take numCoeffsDiv2 [0 ..]
        inBuf     :: VS.Vector Float
        inBuf     =  VG.fromList $ take size [0 ..]
        inBufComplex :: VS.Vector (Complex Float)
        inBufComplex =  VG.fromList $ take size $ do
            i <- [0..]
            return $ i :+ i

        numConv   = 16386
        inBufConv :: VS.Vector CUChar
        inBufConv = VG.fromList $ take size $ concat $ repeat [0 .. 255]

        duplicate :: [a] -> [a]
        duplicate = concat . map func 
            where func x = [x, x]

        coeffs2 :: VS.Vector Float
        coeffs2 =  VG.fromList $ duplicate $ take numCoeffs [0 ..]

    outBuf        :: VS.MVector RealWorld Float <- VGM.new size
    outBufComplex :: VS.MVector RealWorld (Complex Float) <- VGM.new size

    --Benchmarks
    defaultMain [
            bgroup "filter" [
                bgroup "real" [
                    bench "highLevel"   $ nfIO $ filterHighLevel        num coeffs inBuf outBuf,
                    bench "imperative1" $ nfIO $ filterImperative1      num coeffs inBuf outBuf,
                    bench "imperative2" $ nfIO $ filterImperative2      num coeffs inBuf outBuf,
                    bench "c"           $ nfIO $ filterCRR              num coeffs inBuf outBuf,
                    bench "cSSE"        $ nfIO $ filterCSSERR           num coeffs inBuf outBuf,
                    bench "cSSESym"     $ nfIO $ filterCSSESymmetricRR  num coeffsSym inBuf outBuf,
                    bench "cAVX"        $ nfIO $ filterCAVXRR           num coeffs inBuf outBuf,
                    bench "cAVXSym"     $ nfIO $ filterCAVXSymmetricRR  num coeffsSym inBuf outBuf
                ],
                bgroup "complex" [
                    bench "highLevel"   $ nfIO $ filterHighLevel        num coeffs  inBufComplex outBufComplex,
                    bench "c"           $ nfIO $ filterCRC              num coeffs  inBufComplex outBufComplex,
                    bench "cSSE"        $ nfIO $ filterCSSERC           num coeffs2 inBufComplex outBufComplex,
                    bench "cSSE2"       $ nfIO $ filterCSSERC2          num coeffs  inBufComplex outBufComplex,
                    bench "cAVX"        $ nfIO $ filterCAVXRC           num coeffs2 inBufComplex outBufComplex
                ]
            ],
            bgroup "decimate" [
                bgroup "real" [
                    bench "highLevel"   $ nfIO $ decimateHighLevel        (num `quot` decimation) decimation coeffs inBuf outBuf,
                    bench "c"           $ nfIO $ decimateCRR              (num `quot` decimation) decimation coeffs inBuf outBuf,
                    bench "cSSE"        $ nfIO $ decimateCSSERR           (num `quot` decimation) decimation coeffs inBuf outBuf,
                    bench "cSSESym"     $ nfIO $ decimateCSSESymmetricRR  (num `quot` decimation) decimation coeffsSym inBuf outBuf,
                    bench "cAVX"        $ nfIO $ decimateCAVXRR           (num `quot` decimation) decimation coeffs inBuf outBuf,
                    bench "cAVXSym"     $ nfIO $ decimateCAVXSymmetricRR  (num `quot` decimation) decimation coeffsSym inBuf outBuf
                ],
                bgroup "complex" [
                    bench "highLevel"   $ nfIO $ decimateHighLevel      (num `quot` decimation) decimation coeffs  inBufComplex outBufComplex,
                    bench "c"           $ nfIO $ decimateCRC            (num `quot` decimation) decimation coeffs  inBufComplex outBufComplex,
                    bench "cSSE"        $ nfIO $ decimateCSSERC         (num `quot` decimation) decimation coeffs2 inBufComplex outBufComplex,
                    bench "cSSE2"       $ nfIO $ decimateCSSERC2        (num `quot` decimation) decimation coeffs  inBufComplex outBufComplex,
                    bench "cAVX"        $ nfIO $ decimateCAVXRC         (num `quot` decimation) decimation coeffs2 inBufComplex outBufComplex
                ]
            ],
            bgroup "resample" [
                bgroup "real" [
                    bench "highLevel"   $ nfIO $ resampleHighLevel      num interpolation decimation 0 coeffs inBuf outBuf,
                    bench "c"           $ nfIO $ resampleC              num interpolation decimation 0 coeffs inBuf outBuf
                ],
                bgroup "complex" [
                    bench "highLevel"   $ nfIO $ resampleHighLevel      num interpolation decimation 0 coeffs inBufComplex outBufComplex
                ]
            ],
            bgroup "conversion" [
                bench "c"               $ nfIO $ convertC numConv inBufConv outBuf
                --bench "c"               $ nfIO $ convertHighLevel  inBufConv
            ],
            bgroup "scaling" [
                bench "c"               $ nfIO $ scaleC    size 0.3 outBuf,
                bench "cSSE"            $ nfIO $ scaleCSSE size 0.3 outBuf,
                bench "cAVX"            $ nfIO $ scaleCAVX size 0.3 outBuf
            ]
        ]

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
        r7 <- run $ getResult num $ filterCSSESymmetricRR num vCoeffsHalf vInput
        r8 <- run $ getResult num $ filterCAVXSymmetricRR num vCoeffsHalf vInput

        assert $ and $ map (r1 `eqDelta`) [r2, r3, r4, r5, r6, r7, r8]
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
        r5 <- run $ getResult num $ filterCSSESymmetricRC num vCoeffsHalf vInput
        r6 <- run $ getResult num $ filterCAVXRC          num vCoeffs2    vInput

        assert $ and $ map (r1 `eqDeltaC`) [r2, r3, r4, r6]
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
        r5 <- run $ getResult num $ decimateCSSESymmetricRR num factor vCoeffsHalf vInput
        r6 <- run $ getResult num $ decimateCAVXSymmetricRR num factor vCoeffsHalf vInput

        assert $ and $ map (r1 `eqDelta`) [r2, r3, r4, r5, r6]
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

main = theTest
