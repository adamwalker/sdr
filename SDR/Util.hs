{-# LANGUAGE GADTs, FlexibleContexts, BangPatterns #-}

module SDR.Util where

import Control.Monad
import Foreign.Storable
import Foreign.C.Types
import Foreign.Ptr
import Foreign.ForeignPtr
import Data.Complex
import Foreign.Storable.Complex
import Data.ByteString.Internal
import System.IO
import Data.Time.Clock
import Foreign.Marshal.Array
import Data.Vector.Generic as VG
import Data.Vector.Storable as VS
import Data.Vector.Fusion.Stream.Monadic
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

printStream :: (Show e, Storable e) => Int -> Consumer (ForeignPtr e) IO ()
printStream samples = forever $ do
    res <- await 
    res <- lift $ withForeignPtr res $ peekArray samples
    lift $ print res

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
makeComplexBufferVect :: (VG.Vector v1 CUChar, VG.Vector v2 (Complex CDouble)) => Int -> v1 CUChar -> v2 (Complex CDouble)
makeComplexBufferVect samples input = VG.generate samples convert
    where
    {-# INLINE convert #-}
    convert idx  = convert' (input `VG.unsafeIndex` idx) :+ convert' (input `VG.unsafeIndex` (idx + 1))
    {-# INLINE convert' #-}
    convert' val = (fromIntegral val - 128) / 128

toByteString :: Int -> Pipe (ForeignPtr a) ByteString IO ()
toByteString bytes = P.map $ \dat -> PS (castForeignPtr dat) 0 bytes

fromByteString :: Pipe ByteString (ForeignPtr a) IO ()
fromByteString = P.map $ \(PS fp _ _) -> castForeignPtr fp

toHandle :: Int -> Handle -> Consumer (ForeignPtr a) IO ()
toHandle bytes handle = toByteString bytes >-> PB.toHandle handle 

fromHandle :: Int -> Handle -> Producer (ForeignPtr a) IO ()
fromHandle bytes handle = PB.hGet bytes handle >-> fromByteString 

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

