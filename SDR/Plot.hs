module SDR.Plot where

import Control.Monad
import Foreign.C.Types
import Control.Monad.Trans.Either
import Data.Complex
import Foreign.Marshal.Array
import Foreign.ForeignPtr
import Foreign.Storable.Complex

import Pipes

import DynamicGraph.SimpleLine

plot :: Int -> CFloat -> EitherT String IO (Consumer (ForeignPtr (Complex CDouble)) IO ())
plot samples gain = do
    graphFunc <- graph
    let xCoords = take samples $ iterate (+ (2 / fromIntegral samples)) (-1)
    return $ forever $ do
        dat <- await
        lift $ withForeignPtr dat $ \dp -> do
            e <- peekArray samples dp
            let mags = map ((* gain) . realToFrac . magnitude) e
            let interleave = concatMap (\(x, y) -> [x, y])
            graphFunc $ interleave $ zip xCoords mags

