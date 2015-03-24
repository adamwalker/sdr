{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances, ScopedTypeVariables, BangPatterns, RecordWildCards #-}

import           Control.Monad.Primitive 
import           Control.Monad
import           Foreign.C.Types
import           Foreign.Ptr
import           Unsafe.Coerce
import           Data.Complex
import           Foreign.Marshal.Array

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

pad :: a -> Int -> [a] -> [a]
pad with num list = list ++ replicate (num - length list) with 

strideList :: Int -> [a] -> [a]
strideList s xs = go 0 xs
    where
    go _ []     = []
    go 0 (x:xs) = x : go (s-1) xs
    go n (x:xs) = go (n - 1) xs

roundUp :: Int -> Int -> Int
roundUp num div = ((num + div - 1) `quot` div) * div

data Coeffs = Coeffs {
    numCoeffs  :: Int,
    numGroups  :: Int,
    increments :: [Int],
    groups     :: [[Float]]
}

prepareCoeffs :: Int -> Int -> Int -> [Float] -> Coeffs
prepareCoeffs n interpolation decimation coeffs = Coeffs {..}
    where
    numCoeffs   = maximum $ map (length . snd) dats
    numGroups   = length groups
    increments  = map fst dats

    groups      :: [[Float]]
    groups      = map (pad 0 (roundUp numCoeffs n)) $ map snd dats

    dats :: [(Int, [Float])]
    dats = func 0
        where

        func' 0      = []
        func' x      = func x

        func :: Int -> [(Int, [Float])]
        func offset = (increment, strideList interpolation $ drop offset coeffs) : func' offset'
            where
            (q, r)    = quotRem (decimation - offset - 1) interpolation
            increment = q + 1
            offset'   = interpolation - 1 - r

resampleFFIR :: (Ptr CFloat -> Ptr CFloat -> IO ()) -> VS.Vector Float -> VSM.MVector RealWorld Float -> IO ()
resampleFFIR func inBuf outBuf = 
    VS.unsafeWith (unsafeCoerce inBuf) $ \iPtr -> 
        VS.unsafeWith (unsafeCoerce outBuf) $ \oPtr -> 
            func iPtr oPtr

type ResampleR = CInt -> CInt -> CInt -> CInt -> Ptr CInt -> Ptr (Ptr CFloat) -> Ptr CFloat -> Ptr CFloat -> IO ()

mkResampler :: ResampleR -> Int -> Int -> Int -> [Float] -> IO (Int -> Int -> VS.Vector Float -> VS.MVector RealWorld Float -> IO ())
mkResampler func n interpolation decimation coeffs = do
    groupsP     <- mapM newArray $ map (map realToFrac) groups
    groupsPP    <- newArray groupsP
    incrementsP <- newArray $ map fromIntegral increments
    return $ \num offset -> resampleFFIR $ func (fromIntegral num) (fromIntegral numCoeffs) (fromIntegral offset) (fromIntegral numGroups) incrementsP groupsPP
    where
    Coeffs {..} = prepareCoeffs n interpolation decimation coeffs

-- | Scaling
foreign import ccall unsafe "scale"
    scale_c :: CInt -> CFloat -> Ptr CFloat -> Ptr CFloat -> IO ()

scaleC :: Int -> Float -> VS.Vector Float -> VS.MVector RealWorld Float -> IO ()
scaleC num factor inBuf outBuf = 
    VS.unsafeWith (unsafeCoerce inBuf) $ \iPtr -> 
        VS.unsafeWith (unsafeCoerce outBuf) $ \oPtr -> 
            scale_c (fromIntegral num) (unsafeCoerce factor) iPtr oPtr

foreign import ccall unsafe "scaleSSE"
    scaleSSE_c :: CInt -> CFloat -> Ptr CFloat -> Ptr CFloat-> IO ()

scaleCSSE :: Int -> Float -> VS.Vector Float -> VS.MVector RealWorld Float -> IO ()
scaleCSSE num factor inBuf outBuf = 
    VS.unsafeWith (unsafeCoerce inBuf) $ \iPtr -> 
        VS.unsafeWith (unsafeCoerce outBuf) $ \oPtr -> 
            scaleSSE_c (fromIntegral num) (unsafeCoerce factor) iPtr oPtr

foreign import ccall unsafe "scaleAVX"
    scaleAVX_c :: CInt -> CFloat -> Ptr CFloat -> Ptr CFloat -> IO ()

scaleCAVX :: Int -> Float -> VS.Vector Float -> VS.MVector RealWorld Float -> IO ()
scaleCAVX num factor inBuf outBuf = 
    VS.unsafeWith (unsafeCoerce inBuf) $ \iPtr -> 
        VS.unsafeWith (unsafeCoerce outBuf) $ \oPtr -> 
            scaleAVX_c (fromIntegral num) (unsafeCoerce factor) iPtr oPtr

theBench :: IO ()
theBench = do
    --Setup
    let size       =  16384
        numCoeffs  =  128
        num        =  size - numCoeffs + 1
        decimation =  4
        interpolation = 3
        numCoeffsDiv2  =  64

        coeffsList :: [Float]
        coeffsList = take numCoeffs [0 ..]
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
                    bench "highLevel"   $ nfIO $ filterHighLevel        num coeffs inBuf outBuf
                ],
                bgroup "complex" [
                    bench "highLevel"   $ nfIO $ filterHighLevel        num coeffs  inBufComplex outBufComplex
                ]
            ],
            bgroup "decimate" [
                bgroup "real" [
                    bench "highLevel"   $ nfIO $ decimateHighLevel        (num `quot` decimation) decimation coeffs inBuf outBuf
                ],
                bgroup "complex" [
                    bench "highLevel"   $ nfIO $ decimateHighLevel      (num `quot` decimation) decimation coeffs  inBufComplex outBufComplex
                ]
            ],
            bgroup "resample" [
                bgroup "real" [
                    bench "highLevel"   $ nfIO $ resampleHighLevel      (num `quot` decimation) interpolation decimation 0 coeffs inBuf outBuf
                ],
                bgroup "complex" [
                    bench "highLevel"   $ nfIO $ resampleHighLevel      (num `quot` decimation) interpolation decimation 0 coeffs inBufComplex outBufComplex
                ]
            ],
            bgroup "scaling" [
                bench "c"               $ nfIO $ scaleC    size 0.3 inBuf outBuf,
                bench "cSSE"            $ nfIO $ scaleCSSE size 0.3 inBuf outBuf,
                bench "cAVX"            $ nfIO $ scaleCAVX size 0.3 inBuf outBuf
            ]
        ]

theTest = quickCheck $ conjoin [propFiltersComplex]
    where
    sizes           = elements [1024, 2048, 4096, 8192, 16384, 32768, 65536]
    numCoeffs       = elements [32, 64, 128, 256, 512]
    factors         = elements [1, 2, 3, 4, 7, 9, 12, 15, 21]
    factors'        = [1, 2, 3, 4, 7, 9, 12, 15, 21]
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

        assert $ and $ map (r1 `eqDelta`) [r1]
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
            --vCoeffs2    = VG.fromList $ duplicate $ coeffs ++ reverse coeffs

        r1 <- run $ getResult num $ filterHighLevel       num vCoeffs     vInput

        assert $ and $ map (r1 `eqDeltaC`) [r1]
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

        assert $ and $ map (r1 `eqDelta`) [r1]
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
            --vCoeffs2    = VG.fromList $ duplicate $ coeffs ++ reverse coeffs

        r1 <- run $ getResult num $ decimateHighLevel       num factor vCoeffs     vInput

        assert $ and $ map (r1 `eqDeltaC`) [r1]
    propResamplingReal = forAll sizes $ \size -> 
                             forAll (vectorOf size (choose (-10, 10))) $ \inBuf -> 
                                 forAll numCoeffs $ \numCoeffs -> 
                                     forAll (vectorOf numCoeffs (choose (-10, 10))) $ \coeffs -> 
                                        forAll (elements $ tail factors') $ \decimation -> 
                                            forAll (elements $ filter (< decimation) factors') $ \interpolation -> 
                                                 testResamplingReal size numCoeffs interpolation decimation coeffs inBuf
    testResamplingReal :: Int -> Int -> Int -> Int -> [Float] -> [Float] -> Property
    testResamplingReal size numCoeffs interpolation decimation coeffs inBuf = monadicIO $ do
        let vCoeffsHalf = VS.fromList coeffs
            vCoeffs     = VS.fromList $ coeffs ++ reverse coeffs
            vInput      = VS.fromList inBuf
            num         = (size - numCoeffs*2 + 1) `quot` decimation

        r1 <- run $ getResult num $ resampleHighLevel       num interpolation decimation 0 vCoeffs vInput

        assert $ and $ map (r1 `eqDelta`) [r1]
    scales          = elements [0.1, 0.5, 1, 2, 10]
    propScaleReal = forAll sizes $ \size -> 
                        forAll (vectorOf size (choose (-10, 10))) $ \inBuf -> 
                            forAll scales $ \factor -> 
                                testScaleReal size inBuf factor
    testScaleReal :: Int -> [Float] -> Float -> Property
    testScaleReal size inBuf factor = monadicIO $ do
        let vInput = VS.fromList inBuf

        r1 <- run $ getResult size $ scaleC    size factor vInput
        r2 <- run $ getResult size $ scaleCSSE size factor vInput
        r3 <- run $ getResult size $ scaleCAVX size factor vInput

        assert $ and $ map (r1 `eqDelta`) [r2, r3]
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
    duplicate = concat . map func 
        where func x = [x, x]

main = theBench
