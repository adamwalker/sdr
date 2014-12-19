module SDR.Plot where

import Control.Monad
import Control.Monad.Trans.Either
import Foreign.Marshal.Array
import Foreign.ForeignPtr
import qualified Data.Vector.Storable as VS
import Graphics.Rendering.OpenGL
import Graphics.UI.GLFW as G
import Graphics.Rendering.Cairo
import Control.Concurrent hiding (yield)
import Control.Concurrent.MVar

import Pipes 
import qualified Pipes.Prelude as P

import Data.Colour.Names
import Graphics.Rendering.Pango

import Graphics.DynamicGraph.TextureLine
import Graphics.DynamicGraph.Waterfall  
import Graphics.DynamicGraph.FillLine   
import Graphics.DynamicGraph.Axis
import Graphics.DynamicGraph.RenderCairo

replaceMVar :: MVar a -> a -> IO ()
replaceMVar mv val = do
    tryTakeMVar mv
    putMVar mv val

plotTexture :: Int -> Int -> Int -> Int -> EitherT String IO (Consumer (VS.Vector GLfloat) IO ())
plotTexture = textureLineWindow

plotTextureAxes :: Int -> Int -> Int -> Int -> Render () -> EitherT String IO (Consumer (VS.Vector GLfloat) IO ())
plotTextureAxes width height samples xResolution rm = do
    mv <- lift newEmptyMVar 

    lift $ forkOS $ void $ runEitherT $ do
        --create a window
        res' <- lift $ createWindow width height "" Nothing Nothing
        win <- maybe (left "error creating window") return res'
        lift $ makeContextCurrent (Just win)
        
        --render the graph
        renderFunc <- lift $ renderTextureLine samples xResolution

        --render the axes
        renderAxisFunc <- lift $ renderCairo rm width height

        lift $ forever $ do
            dat <- takeMVar mv

            makeContextCurrent (Just win)
            clear [ColorBuffer]

            blend $= Disabled

            viewport $= (Position 50 50, Size (fromIntegral width - 100) (fromIntegral height - 100))
            renderFunc dat

            blend $= Enabled
            blendFunc $= (SrcAlpha, OneMinusSrcAlpha)

            viewport $= (Position 0 0, Size (fromIntegral width) (fromIntegral height))
            renderAxisFunc

            swapBuffers win

    return $ for cat (lift . replaceMVar mv)

plotWaterfall :: Int -> Int -> Int -> Int -> [GLfloat] -> EitherT String IO (Consumer (VS.Vector GLfloat) IO ())
plotWaterfall = waterfallWindow

--TODO: doesnt work
plotWaterfallAxes :: Int -> Int -> Int -> Int -> [GLfloat] -> Render () -> EitherT String IO (Consumer (VS.Vector GLfloat) IO ())
plotWaterfallAxes windowWidth windowHeight width height colorMap rm = do
    mv <- lift newEmptyMVar

    lift $ forkOS $ void $ runEitherT $ do
        res' <- lift $ createWindow windowWidth windowHeight "" Nothing Nothing
        win <- maybe (left "error creating window") return res'
        lift $ makeContextCurrent (Just win)

        renderPipe <- lift $ renderWaterfall width height colorMap
        
        renderAxisFunc <- lift $ renderCairo rm width height

        let thePipe = (<-<) renderPipe $ forever $ do
                dat <- lift $ takeMVar mv
                lift $ makeContextCurrent (Just win)

                lift $ viewport $= (Position 0 0, Size (fromIntegral width) (fromIntegral height))
                lift renderAxisFunc

                lift $ viewport $= (Position 50 50, Size (fromIntegral width - 100) (fromIntegral height - 100))

                yield dat
                lift $ swapBuffers win
        lift $ runEffect thePipe

    return $ for cat (lift . replaceMVar mv)

plotFill :: Int -> Int -> Int -> [GLfloat] -> EitherT String IO (Consumer (VS.Vector GLfloat) IO ())
plotFill = filledLineWindow 

plotFillAxes :: Int -> Int -> Int -> [GLfloat] -> Render () -> EitherT String IO (Consumer (VS.Vector GLfloat) IO ())
plotFillAxes width height samples colorMap rm = do
    mv <- lift newEmptyMVar 

    lift $ forkOS $ void $ runEitherT $ do
        res' <- lift $ createWindow width height "" Nothing Nothing
        win <- maybe (left "error creating window") return res'
        lift $ makeContextCurrent (Just win)

        renderFunc <- lift $ renderFilledLine samples colorMap
        
        renderAxisFunc <- lift $ renderCairo rm width height

        lift $ forever $ do
            dat <- takeMVar mv

            makeContextCurrent (Just win)
            clear [ColorBuffer]

            viewport $= (Position 50 50, Size (fromIntegral width - 100) (fromIntegral height - 100))
            renderFunc dat

            blend $= Enabled
            blendFunc $= (SrcAlpha, OneMinusSrcAlpha)

            viewport $= (Position 0 0, Size (fromIntegral width) (fromIntegral height))
            renderAxisFunc

            swapBuffers win

    return $ for cat (lift . replaceMVar mv)

zeroAxes :: Int -> Int -> Double -> Double -> Render ()
zeroAxes width height bandwidth interval = do
    blankCanvasAlpha black 0 (fromIntegral width) (fromIntegral height) 
    let xSeparation = (interval / bandwidth) * (fromIntegral width - 100)
        ySeparation = 0.2 * (fromIntegral height - 100)
        xCoords     = takeWhile (< (fromIntegral width  - 50)) $ iterate (+ xSeparation) 50
        yCoords     = takeWhile (> 50) $ iterate (\x -> x - ySeparation) (fromIntegral height - 50)
    ctx <- liftIO $ cairoCreateContext Nothing
    xAxisLabels ctx white (map (\n -> show n ++ " KHz" ) (takeWhile (< bandwidth) $ iterate (+ interval) 0)) xCoords (fromIntegral height - 50)
    drawAxes (fromIntegral width) (fromIntegral height) 50 50 50 50 white 2
    xAxisGrid gray 1 [] 50 (fromIntegral height - 50) xCoords
    yAxisGrid gray 1 [4, 2] 50 (fromIntegral width - 50)  yCoords

centeredAxes :: Int -> Int -> Double -> Double -> Double -> Render ()
centeredAxes width height cFreq bandwidth interval = do
    blankCanvasAlpha black 0 (fromIntegral width) (fromIntegral height) 
    let xSeparation = (interval / bandwidth) * (fromIntegral width - 100)
        firstXLabel = fromIntegral (ceiling ((cFreq - (bandwidth / 2)) / interval)) * interval
        fract x     = x - fromIntegral (floor x)
        xOffset     = fract ((cFreq - (bandwidth / 2)) / interval) * xSeparation
        ySeparation = 0.2 * (fromIntegral height - 100)
        xCoords     = takeWhile (< (fromIntegral width - 50)) $ iterate (+ xSeparation) (50 + xOffset)
        yCoords     = takeWhile (> 50) $ iterate (\x -> x - ySeparation) (fromIntegral height - 50)
    ctx <- liftIO $ cairoCreateContext Nothing
    xAxisLabels ctx white (map (\n -> show n ++ " MHZ") (takeWhile (< (cFreq + bandwidth / 2)) $ iterate (+ interval) firstXLabel)) xCoords (fromIntegral height - 50)
    drawAxes (fromIntegral width) (fromIntegral height) 50 50 50 50 white 2
    xAxisGrid gray 1 [] 50 (fromIntegral height - 50) xCoords
    yAxisGrid gray 1 [4, 2] 50 (fromIntegral width - 50)  yCoords

