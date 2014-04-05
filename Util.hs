{-# LANGUAGE GADTs #-}

module Util where

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

import Pipes
import qualified Pipes.Prelude as P
import qualified Pipes.ByteString as PB

import Buffer

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
foreign import ccall unsafe "convertArray"
    c_convertArray :: CInt -> Ptr CUChar -> Ptr (Complex CDouble) -> IO ()

makeComplexBuffer :: Int -> ForeignPtr CUChar -> IO (ForeignPtr (Complex CDouble))
makeComplexBuffer samples ina = do
    oArray <- mallocForeignBufferAligned samples
    withForeignPtr oArray $ \op -> 
        withForeignPtr ina $ \inp -> do
            c_convertArray (fromIntegral samples * 2) inp op
            return oArray

foreign import ccall unsafe "doubleToFloat"
    c_doubleToFloat :: CInt -> Ptr CDouble -> Ptr CFloat -> IO ()

doubleToFloat :: Int -> ForeignPtr CDouble -> IO (ForeignPtr CFloat)
doubleToFloat samples ina = do
    oArray <- mallocForeignBufferAligned samples
    withForeignPtr oArray $ \op -> 
        withForeignPtr ina $ \inp -> do
            c_doubleToFloat (fromIntegral samples) inp op
            return oArray

toByteString :: Int -> Pipe (ForeignPtr a) ByteString IO ()
toByteString bytes = P.map $ \dat -> PS (castForeignPtr dat) 0 bytes

fromByteString :: Pipe ByteString (ForeignPtr a) IO ()
fromByteString = P.map $ \(PS fp _ _) -> castForeignPtr fp

toHandle :: Int -> Handle -> Consumer (ForeignPtr a) IO ()
toHandle bytes handle = toByteString bytes >-> PB.toHandle handle 

fromHandle :: Int -> Handle -> Producer (ForeignPtr a) IO ()
fromHandle bytes handle = PB.hGet bytes handle >-> fromByteString 

foreign import ccall unsafe "multiplyConstFF"
    c_multiplyConstFF :: CInt -> CDouble -> Ptr CDouble -> Ptr CDouble -> IO ()

multiplyConstFF :: Int -> CDouble -> ForeignPtr CDouble -> IO (ForeignPtr CDouble)
multiplyConstFF samples gain ina = do
    oArray <- mallocForeignBufferAligned samples
    withForeignPtr oArray $ \op -> 
        withForeignPtr ina $ \inp -> do
            c_multiplyConstFF (fromIntegral samples) gain inp op
            return oArray

