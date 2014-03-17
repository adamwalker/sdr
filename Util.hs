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

import Pipes

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

makeComplexBuffer :: Int -> ForeignPtr CUChar -> IO (ForeignPtr (Complex CDouble))
makeComplexBuffer samples ina = do
    oArray <- mallocForeignPtrArray samples
    withForeignPtr oArray $ \op -> 
        withForeignPtr ina $ \inp -> do
            c_convertArray (fromIntegral samples * 2) inp op
            return oArray

