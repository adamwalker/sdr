module SDR.Plot where

import Control.Monad
import Foreign.C.Types
import Control.Monad.Trans.Either
import Data.Complex
import Foreign.Marshal.Array
import Foreign.ForeignPtr
import Foreign.Storable.Complex

import qualified Data.Vector.Storable as VS

import Graphics.Rendering.OpenGL

import Pipes 
import qualified Pipes.Prelude as P

import Graphics.DynamicGraph.SimpleLine  as SL
import Graphics.DynamicGraph.TextureLine as TL
import Graphics.DynamicGraph.Waterfall   as WF
import Graphics.DynamicGraph.FillLine    as FL

plotSimple :: Int -> Int -> Int -> EitherT String IO (Consumer (VS.Vector GLfloat) IO ())
plotSimple width height samples = do
    graphFunc <- SL.graph width height samples
    let xCoords = take samples $ iterate (+ (2 / fromIntegral samples)) (-1)
    return $ forever $ do
        dat <- await
        let (fp, offset, length) = VS.unsafeToForeignPtr dat
        lift $ withForeignPtr fp $ \dp -> do
            e <- peekArray length (advancePtr dp offset)
            let interleave = concatMap (\(x, y) -> [x, y])
            withArray (interleave $ zip xCoords e) graphFunc 

plotTexture :: Int -> Int -> Int -> Int -> EitherT String IO (Consumer (VS.Vector GLfloat) IO ())
plotTexture width height samples xResolution = do
    graphFunc <- TL.graph width height samples xResolution
    return $ forever $ await >>= lift . graphFunc

plotWaterfall :: Int -> Int -> Int -> Int -> [GLfloat] -> EitherT String IO (Consumer (VS.Vector GLfloat) IO ())
plotWaterfall = WF.graph 

plotFill :: Int -> Int -> Int -> [GLfloat] -> EitherT String IO (Consumer (VS.Vector GLfloat) IO ())
plotFill width height samples colorMap = do
    graphFunc <- FL.graph width height samples colorMap
    return $ forever $ await >>= lift . graphFunc
