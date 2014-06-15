module SDR.Plot where

import Control.Monad
import Foreign.C.Types
import Control.Monad.Trans.Either
import Data.Complex
import Foreign.Marshal.Array
import Foreign.ForeignPtr
import Foreign.Storable.Complex

import Graphics.Rendering.OpenGL

import Pipes

import Graphics.DynamicGraph.SimpleLine  as SL
import Graphics.DynamicGraph.TextureLine as TL
import Graphics.DynamicGraph.Waterfall   as WF

plotSimple :: Int -> Int -> Int -> CFloat -> EitherT String IO (Consumer (ForeignPtr (Complex CDouble)) IO ())
plotSimple width height samples gain = do
    graphFunc <- SL.graph width height samples
    let xCoords = take samples $ iterate (+ (2 / fromIntegral samples)) (-1)
    return $ forever $ do
        dat <- await
        lift $ withForeignPtr dat $ \dp -> do
            e <- peekArray samples dp
            let mags = map ((* gain) . realToFrac . magnitude) e
            let interleave = concatMap (\(x, y) -> [x, y])
            withArray (interleave $ zip xCoords mags) graphFunc 

plotTexture :: Int -> Int -> Int -> Int -> CFloat -> EitherT String IO (Consumer (ForeignPtr (Complex CDouble)) IO ())
plotTexture width height samples xResolution gain = do
    graphFunc <- TL.graph width height samples xResolution
    return $ forever $ do
        dat <- await
        lift $ withForeignPtr dat $ \dp -> do
            e <- peekArray samples dp
            let mags = map ((* gain) . realToFrac . magnitude) e
            graphFunc mags

plotWaterfall :: Int -> Int -> Int -> Int -> [GLfloat] -> CFloat -> EitherT String IO (Consumer (ForeignPtr (Complex CDouble)) IO ())
plotWaterfall windowWidth windowHeight samples height colorMap gain = do
    graphPipe <- WF.graph windowWidth windowHeight samples height colorMap
    let process = do
            dat <- await
            mags <- lift $ withForeignPtr dat $ \dp -> do
                e <- peekArray samples dp
                let mags = map ((* gain) . realToFrac . magnitude) e
                return mags
            yield mags
            process 
    return $ process >-> graphPipe

