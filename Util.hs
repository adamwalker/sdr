{-# LANGUAGE GADTs #-}

module Util where

import Data.Array.MArray
import Foreign.Storable
import Control.Monad
import Foreign.C.Types
import Foreign.Ptr
import Data.Array.Storable
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

makeComplexBuffer :: Int -> StorableArray Int CUChar -> IO (StorableArray Int (Complex CDouble))
makeComplexBuffer samples ina = do
    oArray <- newArray_ (0, samples - 1) 
    withStorableArray oArray $ \op -> 
        withStorableArray ina $ \inp -> do
            c_convertArray (fromIntegral samples * 2) inp op
            return oArray

