module SDR.Plot where

import Control.Monad
import Control.Monad.Trans.Either
import Foreign.Marshal.Array
import Foreign.ForeignPtr
import qualified Data.Vector.Storable as VS
import Graphics.Rendering.OpenGL
import Graphics.UI.GLFW as G

import Pipes 
import qualified Pipes.Prelude as P

import Graphics.DynamicGraph.SimpleLine 
import Graphics.DynamicGraph.TextureLine
import Graphics.DynamicGraph.Waterfall  
import Graphics.DynamicGraph.FillLine   
import Graphics.DynamicGraph.Axis
import Graphics.DynamicGraph.RenderCairo

plotSimple :: Int -> Int -> Int -> EitherT String IO (Consumer (VS.Vector GLfloat) IO ())
plotSimple width height samples = do
    graphFunc <- simpleLineWindow width height samples
    let xCoords = take samples $ iterate (+ (2 / fromIntegral samples)) (-1)
    return $ for cat $ \dat -> do
        let (fp, offset, length) = VS.unsafeToForeignPtr dat
        lift $ withForeignPtr fp $ \dp -> do
            e <- peekArray length (advancePtr dp offset)
            let interleave = concatMap (\(x, y) -> [x, y])
            withArray (interleave $ zip xCoords e) graphFunc 

plotSimpleAxes :: Int -> Int -> Int -> EitherT String IO (Consumer (VS.Vector GLfloat) IO ())
plotSimpleAxes width height samples = do
    res' <- lift $ createWindow width height "" Nothing Nothing
    win <- maybe (left "error creating window") return res'
    lift $ makeContextCurrent (Just win)

    renderFunc <- lift $ renderSimpleLine samples
    let xCoords = take samples $ iterate (+ (2 / fromIntegral samples)) (-1)
    
    --render the axes
    let rm = renderAxes (defaultConfiguration {width = fromIntegral width, height = fromIntegral height})
    renderAxisFunc <- lift $ renderCairo rm width height

    return $ for cat $ \dat -> 
        lift $ do
            makeContextCurrent (Just win)

            viewport $= (Position 0 0, Size (fromIntegral width) (fromIntegral height))
            renderAxisFunc

            viewport $= (Position 50 50, Size (fromIntegral width - 100) (fromIntegral height - 100))

            let (fp, offset, length) = VS.unsafeToForeignPtr dat
            withForeignPtr fp $ \dp -> do
                e <- peekArray length (advancePtr dp offset)
                let interleave = concatMap (\(x, y) -> [x, y])
                withArray (interleave $ zip xCoords e) renderFunc

            swapBuffers win

plotTexture :: Int -> Int -> Int -> Int -> EitherT String IO (Consumer (VS.Vector GLfloat) IO ())
plotTexture width height samples xResolution = do
    renderFunc <- textureLineWindow width height samples xResolution
    return $ for cat (lift . renderFunc)

plotTextureAxes :: Int -> Int -> Int -> Int -> EitherT String IO (Consumer (VS.Vector GLfloat) IO ())
plotTextureAxes width height samples xResolution = do
    --create a window
    res' <- lift $ createWindow width height "" Nothing Nothing
    win <- maybe (left "error creating window") return res'
    lift $ makeContextCurrent (Just win)
    
    --render the graph
    renderFunc <- lift $ renderTextureLine samples xResolution

    --render the axes
    let rm = renderAxes (defaultConfiguration {width = fromIntegral width, height = fromIntegral height})
    renderAxisFunc <- lift $ renderCairo rm width height

    return $ for cat $ \dat -> 
        lift $ do
            makeContextCurrent (Just win)

            viewport $= (Position 0 0, Size (fromIntegral width) (fromIntegral height))
            renderAxisFunc

            viewport $= (Position 50 50, Size (fromIntegral width - 100) (fromIntegral height - 100))
            renderFunc dat

            swapBuffers win

plotWaterfall :: Int -> Int -> Int -> Int -> [GLfloat] -> EitherT String IO (Consumer (VS.Vector GLfloat) IO ())
plotWaterfall = waterfallWindow

plotWaterfallAxes :: Int -> Int -> Int -> Int -> [GLfloat] -> EitherT String IO (Consumer (VS.Vector GLfloat) IO ())
plotWaterfallAxes windowWidth windowHeight width height colorMap = do
    res' <- lift $ createWindow windowWidth windowHeight "" Nothing Nothing
    win <- maybe (left "error creating window") return res'
    lift $ makeContextCurrent (Just win)

    renderPipe <- lift $ renderWaterfall width height colorMap
    
    --render the axes
    let rm = renderAxes (defaultConfiguration {width = fromIntegral width, height = fromIntegral height})
    renderAxisFunc <- lift $ renderCairo rm width height

    return $ (<-<) renderPipe $ for cat $ \dat -> do
        lift $ do
            makeContextCurrent (Just win)

            viewport $= (Position 0 0, Size (fromIntegral width) (fromIntegral height))
            renderAxisFunc

            viewport $= (Position 50 50, Size (fromIntegral width - 100) (fromIntegral height - 100))

        yield dat
        lift $ swapBuffers win

plotFill :: Int -> Int -> Int -> [GLfloat] -> EitherT String IO (Consumer (VS.Vector GLfloat) IO ())
plotFill width height samples colorMap = do
    graphFunc <- filledLineWindow width height samples colorMap
    return $ for cat (lift . graphFunc)

plotFillAxes :: Int -> Int -> Int -> [GLfloat] -> EitherT String IO (Consumer (VS.Vector GLfloat) IO ())
plotFillAxes width height samples colorMap = do
    res' <- lift $ createWindow width height "" Nothing Nothing
    win <- maybe (left "error creating window") return res'
    lift $ makeContextCurrent (Just win)

    renderFunc <- lift $ renderFilledLine samples colorMap
    
    --render the axes
    let rm = renderAxes (defaultConfiguration {width = fromIntegral width, height = fromIntegral height})
    renderAxisFunc <- lift $ renderCairo rm width height

    return $ for cat $ \dat -> 
        lift $ do
            makeContextCurrent (Just win)

            viewport $= (Position 0 0, Size (fromIntegral width) (fromIntegral height))
            renderAxisFunc

            viewport $= (Position 50 50, Size (fromIntegral width - 100) (fromIntegral height - 100))
            renderFunc dat

            swapBuffers win

