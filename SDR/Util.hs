{-# LANGUAGE FlexibleContexts, BangPatterns, ScopedTypeVariables #-}

module SDR.Util where

import Control.Monad
import Foreign.C.Types
import Foreign.ForeignPtr
import Foreign.Storable
import Data.Complex
import Data.ByteString.Internal
import System.IO
import Data.Time.Clock
import Data.Vector.Generic                         as VG
import Data.Vector.Storable                        as VS
import Data.Vector.Fusion.Stream.Monadic
import qualified Data.Vector.Fusion.Stream         as VFS
import qualified Data.Vector.Fusion.Stream.Monadic as VFSM
import Data.Tuple.All

import Pipes
import qualified Pipes.Prelude as P
import qualified Pipes.ByteString as PB

fork :: Monad m => Producer a m r -> Producer a (Producer a m) r
fork prod = runEffect $ hoist (lift . lift) prod >-> fork' 
    where 
    fork' = forever $ do
        res <- await
        lift $ yield res
        lift $ lift $ yield res

printStream :: (Show a, VG.Vector v a) => Int -> Consumer (v a) IO ()
printStream samples = for cat $ VG.mapM_ (lift . print . show) 

devnull :: Monad m => Consumer a m ()
devnull = forever await

rate :: Int -> Pipe a a IO b
rate samples = do
    start <- lift $ getCurrentTime 
    let rate' buffers = do
            res <- await

            time <- lift $ getCurrentTime 
            let diff = diffUTCTime time start 
                diffSecs :: Double
                diffSecs = fromRational $ toRational diff

            lift $ print $ buffers * fromIntegral samples / diffSecs

            yield res
            rate' (buffers + 1)
    rate' 1

--Conversion of sample bytes to doubles
{-# SPECIALIZE INLINE makeComplexBufferVect :: Int -> VS.Vector CUChar -> VS.Vector (Complex CDouble) #-}
makeComplexBufferVect :: (Num a, Integral a, Num b, Fractional b, VG.Vector v1 a, VG.Vector v2 (Complex b)) => Int -> v1 a -> v2 (Complex b)
makeComplexBufferVect samples input = VG.generate samples convert
    where
    {-# INLINE convert #-}
    convert idx  = convert' (input `VG.unsafeIndex` idx) :+ convert' (input `VG.unsafeIndex` (idx + 1))
    {-# INLINE convert' #-}
    convert' val = (fromIntegral val - 128) / 128

toByteString :: forall a. (Storable a) => Pipe (VS.Vector a) ByteString IO ()
toByteString = P.map $ \dat -> let (fp, o, sz) = VS.unsafeToForeignPtr dat in PS (castForeignPtr fp) o (sz * sizeOf (undefined :: a))

fromByteString :: forall a. (Storable a) => Pipe ByteString (VS.Vector a) IO ()
fromByteString = P.map $ \(PS fp o l) -> unsafeFromForeignPtr (castForeignPtr fp) o (l `quot` sizeOf (undefined :: a))

toHandle :: (Storable a) => Handle -> Consumer (VS.Vector a) IO ()
toHandle handle = toByteString >-> PB.toHandle handle 

fromHandle :: forall a. (Storable a) => Int -> Handle -> Producer (VS.Vector a) IO ()
fromHandle samples handle = PB.hGet (samples * sizeOf (undefined :: a)) handle >-> fromByteString 

{-# INLINE_STREAM mapAccumMV #-}
mapAccumMV :: (Monad m) => (acc -> x -> m (acc, y)) -> acc -> Stream m x -> Stream m y
mapAccumMV func z (Stream step s sz) = Stream step' (s, z) sz
    where
    {-# INLINE_INNER step' #-}
    step' (s, acc) = do
        r <- step s
        case r of
            Yield y s' -> do
                (!acc', !res) <- func acc y 
                return $ Yield res (s', acc')
            Skip    s' -> return $ Skip (s', acc)
            Done       -> return $ Done

{-# INLINE_STREAM stride #-}
stride :: VG.Vector v a => Int -> v a -> v a
stride str inv = VG.unstream $ VFS.unfoldr func 0
    where
    len = VG.length inv
    {-# INLINE_INNER func #-}
    func i | i >= len  = Nothing
           | otherwise = Just (VG.unsafeIndex inv i, i + str)

