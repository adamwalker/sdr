module Plot where

import Control.Monad
import Foreign.C.Types
import Control.Monad.Trans.Either
import Data.Complex
import Data.Array.MArray

import Data.Array.CArray
import Data.Array.CArray.Base
import Pipes

import DynamicGraph.SimpleLine

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

