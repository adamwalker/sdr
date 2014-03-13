{-# LANGUAGE GADTs #-}
module SDRStream where

import Control.Monad
import Control.Monad.Trans.Either
import Data.Bits
import Data.Word

import Data.Array.MArray
import Foreign.Storable
import Foreign.Marshal.Array
import Foreign.Marshal.Alloc
import Foreign.Marshal.Utils
import Data.Array.Storable
import Foreign.C.Types
import Data.Array.CArray
import Data.Array.CArray.Base
import Data.Complex
import Foreign.ForeignPtr
import Foreign.Storable.Complex
import Foreign.Ptr

import Pipes

import RTLSDR
import HFFT
import SimpleLine

--RTLSDR streaming
sdrStream :: Word32 -> Word32 -> Int -> EitherT String IO (Producer (StorableArray Int CUChar) IO ())
sdrStream frequency sampleRate samples = do
    lift $ putStrLn "Initializing RTLSDR device"

    dev' <- lift $ open 0
    dev  <- maybe (left "Failed to open device") return dev'

    lift $ do
        t <- getTunerType dev
        putStrLn $ "Found a: " ++ show t

        setFreqCorrection dev 0
        setSampleRate dev sampleRate
        setCenterFreq dev frequency
        setTunerGainMode dev False

        resetBuffer dev

        return $ mkSdrStream samples dev

mkSdrStream :: Int -> RTLSDR -> Producer (StorableArray Int CUChar) IO ()
mkSdrStream samples dev = do
    res' <- lift $ readSync dev (samples * 2)
    maybe (lift $ print "Stream terminated") (yield >=> const (mkSdrStream samples dev)) res'

--Output dumping
printStream :: (Show e, Storable e, m ~ IO, MArray a e m, Ix i) => Consumer (a i e) m ()
printStream = forever $ do
    res <- await 
    res <- lift $ getElems res
    lift $ print res

devnull :: (Show e, Storable e, m ~ IO, MArray a e m, Ix i) => Consumer (a i e) m ()
devnull = forever await

--Conversion of sample bytes to doubles
foreign import ccall unsafe "convertArray"
    c_convertArray :: CInt -> Ptr CUChar -> Ptr (Complex CDouble) -> IO ()

makeComplexBuffer :: Int -> StorableArray Int CUChar -> IO (StorableArray Int (Complex CDouble))
makeComplexBuffer samples ina = do
    oArray <- newArray_ (0, samples - 1) 
    withStorableArray oArray $ \op -> 
        withStorableArray ina $ \inp -> do
            c_convertArray (fromIntegral samples * 2) inp op
            return oArray

--FFT
mkFFTWArray :: Int -> IO (IOCArray Int (Complex CDouble))
mkFFTWArray samples = do
    memory <- fftwMalloc (fromIntegral $ samples * sizeOf (undefined :: Complex CDouble))
    fp <- newForeignPtr_ memory
    unsafeForeignPtrToIOCArray fp (0, samples - 1) :: IO (IOCArray Int (Complex CDouble))

foreign import ccall unsafe "convertFFT"
    c_convertFFT :: CInt -> Ptr (Complex CDouble) -> Ptr (Complex CDouble) -> IO ()

convertFFT :: Int -> StorableArray Int (Complex CDouble) -> IOCArray Int (Complex CDouble) -> IO ()
convertFFT samples ina out = 
    withStorableArray ina $ \ip -> 
    withIOCArray      out $ \op -> 
        c_convertFFT (fromIntegral samples) ip op

convertForFFT :: Int -> IOCArray Int (Complex CDouble) -> Pipe (StorableArray Int (Complex CDouble)) (IOCArray Int (Complex CDouble)) IO ()
convertForFFT samples out = forever $ do
    res <- await
    lift $ convertFFT samples res out
    yield out

fftw :: Int -> IOCArray Int (Complex CDouble) -> IO (Pipe (StorableArray Int (Complex CDouble)) (IOCArray Int (Complex CDouble)) IO ())
fftw samples array = do
    plan <- withIOCArray array $ \ptr -> 
        planDFT1d samples ptr ptr (-1) (1 `shiftL` 6)
    
    return $ forever $ do
        res <- await
        lift $ convertFFT samples res array
        lift $ execute plan
        yield array

mkFFTWArrayReal :: Int -> IO (IOCArray Int CDouble)
mkFFTWArrayReal samples = do
    memory <- fftwMalloc (fromIntegral $ samples * sizeOf (undefined :: CDouble))
    fp <- newForeignPtr_ memory
    unsafeForeignPtrToIOCArray fp (0, samples - 1) :: IO (IOCArray Int CDouble)

fftwReal :: Int -> IOCArray Int CDouble -> IOCArray Int (Complex CDouble) -> IO (Pipe (StorableArray Int CDouble) (IOCArray Int (Complex CDouble)) IO ())
fftwReal samples ina out = do
    plan <- withIOCArray ina $ \ip -> 
        withIOCArray out $ \op -> 
            planDFTR2C1d samples ip op (1 `shiftL` 6)

    return $ forever $ do
        res <- await
        lift $ withStorableArray res $ \ip -> 
            withIOCArray ina $ \op -> 
                moveBytes op ip (samples * sizeOf (undefined :: CDouble))
        lift $ execute plan
        yield out

--Spectrum analyser plots
plot :: Int -> CFloat -> EitherT String IO (Consumer (IOCArray Int (Complex CDouble)) IO ())
plot samples gain = do
    graphFunc <- graph
    let xCoords = take samples $ iterate (+ (2 / fromIntegral samples)) (-1)
    return $ forever $ do
        dat <- await
        e <- lift $ getElems dat
        let mags = map ((* gain) . realToFrac . magnitude) e
        let interleave = concatMap (\(x, y) -> [x, y])
        lift $ graphFunc $ interleave $ zip xCoords mags

--Filtering
firFilter :: Int -> [Complex CDouble] -> IO (Pipe (StorableArray Int (Complex CDouble)) (StorableArray Int (Complex CDouble)) IO ())
firFilter samples coeffs = do
    c <- newListArray (0, length coeffs - 1) coeffs
    return $ do
        first <- await
        let loop last = do
            this <- await
            out <- lift $ doFilter (length coeffs) c samples last this 
            yield out
            loop this
        loop first

foreign import ccall unsafe "filter"
    c_filter :: CInt -> Ptr (Complex CDouble) -> CInt -> Ptr (Complex CDouble) -> Ptr (Complex CDouble ) -> Ptr (Complex CDouble) -> IO ()

doFilter :: Int -> StorableArray Int (Complex CDouble) -> Int -> StorableArray Int (Complex CDouble) -> StorableArray Int (Complex CDouble) -> IO (StorableArray Int (Complex CDouble))
doFilter coeffsLength coeffs samples lastBuffer thisBuffer = do
    outBuffer <- newArray_ (0, samples - 1)
    withStorableArray coeffs     $ \cp -> 
        withStorableArray lastBuffer $ \lp -> 
        withStorableArray thisBuffer $ \tp -> 
        withStorableArray outBuffer  $ \op -> 
            c_filter (fromIntegral coeffsLength) cp (fromIntegral samples) lp tp op
    return outBuffer
   
--Decimation
decimate :: Int -> Int -> [Complex CDouble] -> IO (Pipe (StorableArray Int (Complex CDouble)) (StorableArray Int (Complex CDouble)) IO ())
decimate factor samples coeffs = do
    c <- newListArray (0, length coeffs - 1) coeffs
    return $ do
        first <- await
        let loop last = do
            this <- await
            out <- lift $ doDecimation factor (length coeffs) c samples last this 
            yield out
            loop this
        loop first

foreign import ccall unsafe "decimate"
    c_decimate :: CInt -> CInt -> Ptr (Complex CDouble) -> CInt -> Ptr (Complex CDouble) -> Ptr (Complex CDouble ) -> Ptr (Complex CDouble) -> IO ()

doDecimation :: Int -> Int -> StorableArray Int (Complex CDouble) -> Int -> StorableArray Int (Complex CDouble) -> StorableArray Int (Complex CDouble) -> IO (StorableArray Int (Complex CDouble))
doDecimation factor coeffsLength coeffs samples lastBuffer thisBuffer = do
    outBuffer <- newArray_ (0, (samples `quot` factor) - 1)
    withStorableArray coeffs     $ \cp -> 
        withStorableArray lastBuffer $ \lp -> 
        withStorableArray thisBuffer $ \tp -> 
        withStorableArray outBuffer  $ \op -> 
            c_decimate (fromIntegral factor) (fromIntegral coeffsLength) cp (fromIntegral samples) lp tp op
    return outBuffer

--Demodulation
foreign import ccall unsafe "fmDemod"
    c_fmDemod :: CInt -> Ptr (Complex CDouble) -> Ptr (Complex CDouble) -> Ptr CDouble -> IO ()

fmDemod :: Int -> StorableArray Int (Complex CDouble) -> IO (StorableArray Int CDouble)
fmDemod samples ina = do
    out <- newArray_ (0, samples - 1)
    withStorableArray ina $ \ip -> 
        withStorableArray out $ \op -> 
        alloca $ \sp -> do
            poke sp (fromIntegral 0)
            c_fmDemod (fromIntegral samples) sp ip op
    return out

foreign import ccall unsafe "filter2_onebuf"
    c_filter2OneBuf :: CInt -> Ptr (Complex CDouble) -> CInt -> Ptr (Complex CDouble) -> Ptr (Complex CDouble) -> IO ()

filter2OneBuf :: Int -> StorableArray Int (Complex CDouble) -> Int -> Int -> StorableArray Int (Complex CDouble) -> Int -> StorableArray Int (Complex CDouble) -> IO ()
filter2OneBuf coeffsLength coeffs num inOffset inBuf outOffset outBuf = 
    withStorableArray coeffs $ \cp -> 
    withStorableArray inBuf  $ \ip -> 
    withStorableArray outBuf $ \op -> 
        c_filter2OneBuf (fromIntegral coeffsLength) cp (fromIntegral num) (plusPtr ip (inOffset * sizeOf (undefined :: Complex CDouble))) (plusPtr op (outOffset * sizeOf (undefined :: Complex CDouble)))

foreign import ccall unsafe "filter2_crossbuf"
    c_filter2CrossBuf :: CInt -> Ptr (Complex CDouble) -> CInt -> CInt -> Ptr (Complex CDouble) -> Ptr (Complex CDouble) -> Ptr (Complex doubles) -> IO ()

filter2CrossBuf :: Int -> StorableArray Int (Complex CDouble) -> Int -> Int -> Int -> StorableArray Int (Complex CDouble) -> StorableArray Int (Complex CDouble) -> Int -> StorableArray Int (Complex CDouble) -> IO ()
filter2CrossBuf coeffsLength coeffs numInput num lastOffset lastBuf nextBuf outOffset outBuf = 
    withStorableArray coeffs  $ \cp -> 
    withStorableArray lastBuf $ \lp -> 
    withStorableArray nextBuf $ \np -> 
    withStorableArray outBuf  $ \op -> 
        c_filter2CrossBuf (fromIntegral coeffsLength) cp (fromIntegral numInput) (fromIntegral num) (plusPtr lp (lastOffset * sizeOf (undefined :: Complex CDouble))) np (plusPtr op (outOffset * sizeOf (undefined :: Complex CDouble)))

filter2 :: Int -> StorableArray Int (Complex CDouble) -> Int -> Int -> Pipe (StorableArray Int (Complex CDouble)) (StorableArray Int (Complex CDouble)) IO ()
filter2 numCoeffs coeffs blockSizeIn blockSizeOut = do
    inBuf  <- await
    outBuf <- lift $ newArray_ (0, blockSizeOut - 1)

    simple inBuf 0 blockSizeIn outBuf 0 blockSizeOut 

    where

    advanceOutBuf bufOut offsetOut spaceOut count = 
        if count == spaceOut then do
            yield bufOut
            outBuf' <- lift $ newArray_ (0, blockSizeOut - 1)
            return (outBuf', 0, blockSizeOut) 
        else 
            return (bufOut, offsetOut + count, spaceOut - count) 

    simple bufIn offsetIn spaceIn bufOut offsetOut spaceOut = do
        let count = min (spaceIn - numCoeffs + 1) spaceOut
        lift $ filter2OneBuf numCoeffs coeffs count offsetIn bufIn offsetOut bufOut

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
        lift $ filter2CrossBuf numCoeffs coeffs spaceLast count offsetLast bufLast bufNext offsetOut bufOut

        (bufOut', offsetOut', spaceOut') <- advanceOutBuf bufOut offsetOut spaceOut count

        case spaceLast - 1 == count of 
            True  -> do
                simple bufNext 0 blockSizeIn bufOut' offsetOut' spaceOut'
            False -> crossover bufLast (offsetLast + count) (spaceLast - count) bufNext bufOut' offsetOut' spaceOut'

foreign import ccall unsafe "decimate2_onebuf"
    c_decimate2OneBuf :: CInt -> CInt -> Ptr (Complex CDouble) -> CInt -> Ptr (Complex CDouble) -> Ptr (Complex CDouble) -> IO ()

decimate2OneBuf :: Int -> Int -> StorableArray Int (Complex CDouble) -> Int -> Int -> StorableArray Int (Complex CDouble) -> Int -> StorableArray Int (Complex CDouble) -> IO ()
decimate2OneBuf factor coeffsLength coeffs num inOffset inBuf outOffset outBuf = 
    withStorableArray coeffs $ \cp -> 
    withStorableArray inBuf  $ \ip -> 
    withStorableArray outBuf $ \op -> 
        c_decimate2OneBuf (fromIntegral factor) (fromIntegral coeffsLength) cp (fromIntegral num) (plusPtr ip (inOffset * sizeOf (undefined :: Complex CDouble))) (plusPtr op (outOffset * sizeOf (undefined :: Complex CDouble)))

foreign import ccall unsafe "decimate2_crossbuf"
    c_decimate2CrossBuf :: CInt -> CInt -> Ptr (Complex CDouble) -> CInt -> CInt -> Ptr (Complex CDouble) -> Ptr (Complex CDouble) -> Ptr (Complex doubles) -> IO ()

decimate2CrossBuf :: Int -> Int -> StorableArray Int (Complex CDouble) -> Int -> Int -> Int -> StorableArray Int (Complex CDouble) -> StorableArray Int (Complex CDouble) -> Int -> StorableArray Int (Complex CDouble) -> IO ()
decimate2CrossBuf factor coeffsLength coeffs numInput num lastOffset lastBuf nextBuf outOffset outBuf = 
    withStorableArray coeffs  $ \cp -> 
    withStorableArray lastBuf $ \lp -> 
    withStorableArray nextBuf $ \np -> 
    withStorableArray outBuf  $ \op -> 
        c_decimate2CrossBuf (fromIntegral factor) (fromIntegral coeffsLength) cp (fromIntegral numInput) (fromIntegral num) (plusPtr lp (lastOffset * sizeOf (undefined :: Complex CDouble))) np (plusPtr op (outOffset * sizeOf (undefined :: Complex CDouble)))

assert msg False x = error $ "Assertion failed: " ++ msg
assert msg True  x = x

decimate2 :: Int -> Int -> StorableArray Int (Complex CDouble) -> Int -> Int -> Pipe (StorableArray Int (Complex CDouble)) (StorableArray Int (Complex CDouble)) IO ()
decimate2 factor numCoeffs coeffs blockSizeIn blockSizeOut = do
    inBuf  <- await
    outBuf <- lift $ newArray_ (0, blockSizeOut - 1)

    simple inBuf 0 blockSizeIn outBuf 0 blockSizeOut 

    where

    advanceOutBuf bufOut offsetOut spaceOut count = 
        if count == spaceOut then do
            yield bufOut
            outBuf' <- lift $ newArray_ (0, blockSizeOut - 1)
            return (outBuf', 0, blockSizeOut) 
        else 
            return (bufOut, offsetOut + count, spaceOut - count) 

    simple bufIn offsetIn spaceIn bufOut offsetOut spaceOut = do

        assert "" (spaceIn >= numCoeffs) (return ())

        let count = min (((spaceIn - numCoeffs) `quot` factor) + 1) spaceOut
        lift $ decimate2OneBuf factor numCoeffs coeffs count offsetIn bufIn offsetOut bufOut

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
        lift $ decimate2CrossBuf factor numCoeffs coeffs spaceLast count offsetLast bufLast bufNext offsetOut bufOut

        (bufOut', offsetOut', spaceOut') <- advanceOutBuf bufOut offsetOut spaceOut count

        case ((spaceLast - 1) `quot` factor) + 1 == count of 
            True  -> simple bufNext (offsetLast + count * factor - blockSizeIn) (blockSizeIn - (offsetLast + count * factor - blockSizeIn)) bufOut' offsetOut' spaceOut'
            False -> crossover bufLast (offsetLast + count * factor) (spaceLast - count * factor) bufNext bufOut' offsetOut' spaceOut'

foreign import ccall unsafe "resample_onebuf_c"
    c_resampleOneBufC :: CInt -> CInt -> CInt -> Ptr (Complex CDouble) -> CInt -> CInt -> Ptr (Complex CDouble) -> Ptr (Complex CDouble) -> IO CInt

resampleOneBufC :: Int -> Int -> Int -> StorableArray Int (Complex CDouble) -> Int -> Int -> Int -> StorableArray Int (Complex CDouble) -> Int -> StorableArray Int (Complex CDouble) -> IO Int
resampleOneBufC interpolation decimation coeffsLength coeffs filterOffset count inOffset inBuf outOffset outBuf = liftM fromIntegral $ 
    withStorableArray coeffs $ \cp -> 
    withStorableArray inBuf  $ \ip -> 
    withStorableArray outBuf $ \op -> 
        c_resampleOneBufC (fromIntegral interpolation) 
                         (fromIntegral decimation) 
                         (fromIntegral coeffsLength) 
                         cp 
                         (fromIntegral filterOffset) 
                         (fromIntegral count) 
                         (advancePtr ip inOffset)
                         (advancePtr op outOffset)

foreign import ccall unsafe "resample_crossbuf_c"
    c_resampleCrossBufC :: CInt -> CInt -> CInt -> Ptr (Complex CDouble) -> CInt -> CInt -> CInt -> Ptr (Complex CDouble) -> Ptr (Complex CDouble) -> Ptr (Complex doubles) -> IO CInt

resampleCrossBufC :: Int -> Int -> Int -> StorableArray Int (Complex CDouble) -> Int -> Int -> Int -> Int -> StorableArray Int (Complex CDouble) -> StorableArray Int (Complex CDouble) -> Int -> StorableArray Int (Complex CDouble) -> IO Int
resampleCrossBufC interpolation decimation coeffsLength coeffs filterOffset numInput count lastOffset lastBuf nextBuf outOffset outBuf = liftM fromIntegral $ 
    withStorableArray coeffs  $ \cp -> 
    withStorableArray lastBuf $ \lp -> 
    withStorableArray nextBuf $ \np -> 
    withStorableArray outBuf  $ \op -> 
        c_resampleCrossBufC (fromIntegral interpolation) 
                           (fromIntegral decimation) 
                           (fromIntegral coeffsLength) 
                           cp 
                           (fromIntegral filterOffset) 
                           (fromIntegral numInput) 
                           (fromIntegral count) 
                           (plusPtr lp lastOffset)
                           np 
                           (plusPtr op outOffset)

quotUp q d = (q + (d - 1)) `quot` d

resample :: Int -> Int -> Int -> StorableArray Int (Complex CDouble) -> Int -> Int -> Pipe (StorableArray Int (Complex CDouble)) (StorableArray Int (Complex CDouble)) IO ()
resample interpolation decimation numCoeffs coeffs blockSizeIn blockSizeOut = do
    inBuf  <- await
    outBuf <- lift $ newArray_ (0, blockSizeOut - 1)

    simple inBuf 0 blockSizeIn outBuf 0 blockSizeOut 0

    where

    advanceOutBuf bufOut offsetOut spaceOut count = 
        if count == spaceOut then do
            yield bufOut
            outBuf' <- lift $ newArray_ (0, blockSizeOut - 1)
            return (outBuf', 0, blockSizeOut) 
        else 
            return (bufOut, offsetOut + count, spaceOut - count) 

    simple :: StorableArray Int (Complex CDouble) 
           -> Int 
           -> Int 
           -> StorableArray Int (Complex CDouble) 
           -> Int 
           -> Int 
           -> Int 
           -> Pipe (StorableArray Int (Complex CDouble)) (StorableArray Int (Complex CDouble)) IO ()
    simple bufIn offsetIn spaceIn bufOut offsetOut spaceOut filterOffset = do

        --filterOffset is the offset in the filter that the first input data is multiplied with

        --Check that we have space in the input buffer for at least one output
        assert "1" (spaceIn * interpolation >= numCoeffs - filterOffset) (return ())

        --available number of samples == interpolation * num_input
        --required number of samples  == decimation * (num_output - 1) + filter_length - filter_offset
        let count = min (((spaceIn * interpolation - numCoeffs + filterOffset) `quot` decimation) + 1) spaceOut
        endOffset <- lift $ resampleOneBufC interpolation decimation numCoeffs coeffs filterOffset count offsetIn bufIn offsetOut bufOut

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

    crossover :: StorableArray Int (Complex CDouble) 
              -> Int 
              -> Int 
              -> StorableArray Int (Complex CDouble) 
              -> StorableArray Int (Complex CDouble) 
              -> Int 
              -> Int 
              -> Int 
              -> Pipe (StorableArray Int (Complex CDouble)) (StorableArray Int (Complex CDouble)) IO ()
    crossover bufLast offsetLast spaceLast bufNext bufOut offsetOut spaceOut filterOffset = do

        assert "6" (spaceLast > 0) (return ())
        assert "3" (spaceLast * interpolation < numCoeffs - filterOffset) (return ())

        --outputsComputable is the number of outputs that need to be computed for the last buffer to no longer be needed
        --outputsComputable * decimation == numInput * interpolation + filterOffset + k
        let outputsComputable = (spaceLast * interpolation + filterOffset) `quotUp` decimation
            count = min outputsComputable spaceOut
        assert "7" (count /= 0) (return ())

        endOffset <- lift $ resampleCrossBufC interpolation decimation numCoeffs coeffs filterOffset spaceLast count offsetLast bufLast bufNext offsetOut bufOut

        assert "2" ((count * decimation + endOffset - filterOffset) `rem` interpolation == 0) (return ())

        (bufOut', offsetOut', spaceOut') <- advanceOutBuf bufOut offsetOut spaceOut count

        let inputUsed = (count * decimation - filterOffset) `quotUp` interpolation

        case inputUsed >= spaceLast of 
            True  -> simple bufNext (offsetLast + inputUsed - blockSizeIn) (2 * blockSizeIn - (offsetLast + inputUsed)) bufOut' offsetOut' spaceOut' endOffset
            False -> crossover bufLast (offsetLast + inputUsed) (spaceLast - inputUsed) bufNext bufOut' offsetOut' spaceOut' endOffset

