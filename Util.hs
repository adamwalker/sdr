{-# LANGUAGE GADTs #-}

module Util where

import Data.Array.MArray
import Foreign.Storable
import Control.Monad
import Foreign.C.Types
import Foreign.Ptr
import Foreign.ForeignPtr
import Data.Complex
import Foreign.Storable.Complex
import Data.ByteString.Internal
import System.IO

import Pipes
import qualified Pipes.Prelude as P
import qualified Pipes.ByteString as PB

import Buffer

printStream :: (Show e, Storable e, m ~ IO, MArray a e m, Ix i) => Consumer (a i e) m ()
printStream = forever $ do
    res <- await 
    res <- lift $ getElems res
    lift $ print res

devnull :: Monad m => Consumer a m ()
devnull = forever await

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

