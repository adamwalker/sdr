{-# LANGUAGE FlexibleContexts, BangPatterns, ScopedTypeVariables, MultiParamTypeClasses, FlexibleInstances #-}

module SDR.Util where

import Control.Monad
import Foreign.C.Types
import Foreign.ForeignPtr
import Foreign.Storable
import Data.Complex
import Data.ByteString.Internal 
import Data.ByteString as BS
import System.IO
import Data.Time.Clock
import Data.Vector.Generic                         as VG   hiding ((++))
import qualified Data.Vector.Generic.Mutable       as VGM
import Data.Vector.Storable                        as VS   hiding ((++))
import Data.Vector.Fusion.Stream.Monadic                   hiding ((++))
import qualified Data.Vector.Fusion.Stream         as VFS  hiding ((++))
import qualified Data.Vector.Fusion.Stream.Monadic as VFSM hiding ((++))
import Data.Tuple.All
import Control.Monad.Primitive
import Control.Applicative

import Pipes
import qualified Pipes.Prelude as P
import qualified Pipes.ByteString as PB
import Data.Serialize hiding (Done)
import qualified Data.Serialize as S
import Options.Applicative

fork :: Monad m => Producer a m r -> Producer a (Producer a m) r
fork prod = runEffect $ hoist (lift . lift) prod >-> fork' 
    where 
    fork' = forever $ do
        res <- await
        lift $ yield res
        lift $ lift $ yield res

-- | A consumer that prints everything to stdout
printStream :: (Show a, VG.Vector v a) => Int -> Consumer (v a) IO ()
printStream samples = for cat $ VG.mapM_ (lift . print) 

-- | A consumer that discards everything
devnull :: Monad m => Consumer a m ()
devnull = forever await

-- | Passthrough pipe that prints the sample rate
rate :: Int -> Pipe a a IO b
rate samples = do
    start <- lift getCurrentTime 
    let rate' buffers = do
            res <- await

            time <- lift getCurrentTime 
            let diff = diffUTCTime time start 
                diffSecs :: Double
                diffSecs = fromRational $ toRational diff

            lift $ print $ buffers * fromIntegral samples / diffSecs

            yield res
            rate' (buffers + 1)
    rate' 1

{-| Create a vector of complex samples from a vector of interleaved
    I Q components.
-}
{-# INLINE makeComplexBufferVect #-}
makeComplexBufferVect :: (Num a, Integral a, Num b, Fractional b, VG.Vector v1 a, VG.Vector v2 (Complex b)) => Int -> v1 a -> v2 (Complex b)
makeComplexBufferVect samples input = VG.generate samples convert
    where
    {-# INLINE convert #-}
    convert idx  = convert' (input `VG.unsafeIndex` (2 * idx)) :+ convert' (input `VG.unsafeIndex` (2 * idx + 1))
    {-# INLINE convert' #-}
    convert' val = (fromIntegral val - 128) / 128

{-| Slow functions for serializing/deserializing vectors to/from
    bytestrings. There must be a better way to do this that doesn't involve
    copying.
-}
floatVecToByteString    :: VG.Vector v Float  => v Float -> ByteString
floatVecToByteString vect = runPut $ VG.mapM_ putFloat32le vect

doubleVecToByteString   :: VG.Vector v Double => v Double -> ByteString
doubleVecToByteString vect = runPut $ VG.mapM_ putFloat64le vect

floatVecFromByteString  :: VG.Vector v Float  => ByteString -> v Float
floatVecFromByteString bs = VG.unfoldrN (BS.length bs `div` 4) go bs
    where
    go bs = case runGetPartial getFloat32le bs of
                Fail _ _    -> Nothing
                Partial _   -> error "floatVecFromByteString: Partial"
                S.Done r b  -> Just (r, b)

doubleVecFromByteString  :: VG.Vector v Double  => ByteString -> v Double
doubleVecFromByteString bs = VG.unfoldrN (BS.length bs `div` 8) go bs
    where
    go bs = case runGetPartial getFloat64le bs of
                Fail _ _    -> Nothing
                Partial _   -> error "doubleVecFromByteString"
                S.Done r b  -> Just (r, b)

{-| Fast functions for serializing/deserializing storable vectors to/from
    bytestrings.
-}
toByteString :: forall a. (Storable a) => Pipe (VS.Vector a) ByteString IO ()
toByteString = P.map $ \dat -> let (fp, o, sz) = VS.unsafeToForeignPtr dat in PS (castForeignPtr fp) o (sz * sizeOf (undefined :: a))

fromByteString :: forall a. (Storable a) => Pipe ByteString (VS.Vector a) IO ()
fromByteString = P.map $ \(PS fp o l) -> unsafeFromForeignPtr (castForeignPtr fp) o (l `quot` sizeOf (undefined :: a))

toHandle :: (Storable a) => Handle -> Consumer (VS.Vector a) IO ()
toHandle handle = toByteString >-> PB.toHandle handle 

fromHandle :: forall a. (Storable a) => Int -> Handle -> Producer (VS.Vector a) IO ()
fromHandle samples handle = PB.hGet (samples * sizeOf (undefined :: a)) handle >-> fromByteString 

{-| Like mapAccumL but monadic and over vectors. Doesn't return the
    accumulator at the end because it doesn't seem to be possible to do
    this with the Stream datatype, making this function pretty useless.
-}
mapAccumMV :: (Monad m) => (acc -> x -> m (acc, y)) -> acc -> Stream m x -> Stream m y
mapAccumMV func z (Stream step s sz) = Stream step' (s, z) sz
    where
    step' (s, acc) = do
        r <- step s
        case r of
            Yield y s' -> do
                (!acc', !res) <- func acc y 
                return $ Yield res (s', acc')
            Skip    s' -> return $ Skip (s', acc)
            Done       -> return Done

{-| Create a vector from another vector containing only the elements that
    occur every stride elements in the source vector.
-}
{-# INLINE stride #-}
stride :: VG.Vector v a => Int -> v a -> v a
stride str inv = VG.unstream $ VFS.unfoldr func 0
    where
    len = VG.length inv
    func i | i >= len  = Nothing
           | otherwise = Just (VG.unsafeIndex inv i, i + str)

-- | Fill a mutable vector from a monadic stream
{-# INLINE fill #-}
fill :: (PrimMonad m, Functor m, VGM.MVector vm a) => VFS.MStream m a -> vm (PrimState m) a -> m ()
fill str outBuf = void $ VFSM.foldM' put 0 str
    where 
    put i x = do
        VGM.unsafeWrite outBuf i x
        return $ i + 1

-- | A class for things that can be multiplied by a scalar.
class Mult a b where
    mult :: a -> b -> a

instance (Num a) => Mult a a where
    mult = (*)

instance (Num a) => Mult (Complex a) a where
    mult (x :+ y) z = (x * z) :+ (y * z)

parseSize :: ReadM Integer
parseSize = eitherReader $ \arg -> case reads arg of
    [(r, suffix)] -> case suffix of 
        []  -> return r
        "K" -> return $ r * 1000 
        "M" -> return $ r * 1000000
        "G" -> return $ r * 1000000000
        x   -> Left  $ "Cannot parse suffix: `" ++ x ++ "'"
    _             -> Left $ "Cannot parse value: `" ++ arg ++ "'"

