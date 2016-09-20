{-| Filter design and plotting of frequency responses. -}

module SDR.FilterDesign (
    -- * Sinc Function
    sinc,

    -- * Root raised cosine
    srrc,

    -- * Windows
    hanning,
    hamming,
    blackman,

    -- * Convenience Functions
    windowedSinc,
    
    -- * Frequency Response Plot
    plotFrequency
    ) where

import Graphics.Rendering.Chart.Easy
import Graphics.Rendering.Chart.Backend.Cairo
import Data.Complex

import qualified Data.Vector.Generic as VG

-- | Compute a sinc function
sinc :: (Floating n, VG.Vector v n)
     => Int -- ^ The length. Must be odd.
     -> n   -- ^ The cutoff frequency (from 0 to 1)
     -> v n
sinc size cutoff  = VG.generate size (func . (-) ((size - 1) `quot` 2))
    where
    func 0   = cutoff
    func idx = sin (pi * cutoff * fromIntegral idx) / (fromIntegral idx * pi)

-- | Compute a Hanning window.
hanning :: (Floating n, VG.Vector v n) 
        => Int -- ^ The length of the window
        -> v n
hanning size = VG.generate size func
    where
    func idx = 0.5 * (1 - cos((2 * pi * fromIntegral idx) / (fromIntegral size - 1)))
  
-- | Compute a Hamming window. 
hamming :: (Floating n, VG.Vector v n) 
        => Int -- ^ The length of the window
        -> v n
hamming size = VG.generate size func
    where
    func idx = 0.54 - 0.46 * cos((2 * pi * fromIntegral idx) / (fromIntegral size - 1))
   
-- | Compute a Blackman window.
blackman :: (Floating n, VG.Vector v n) 
        => Int -- ^ The length of the window
        -> v n
blackman size = VG.generate size func
    where
    func idx = 0.42 - 0.5 * cos((2 * pi * fromIntegral idx) / (fromIntegral size - 1)) + 0.08 * cos((4 * pi * fromIntegral idx) / (fromIntegral size - 1))

-- | Compute a windowed sinc function
windowedSinc :: (Floating n, VG.Vector v n)
             => Int          -- ^ The length
             -> n            -- ^ The cutoff frequency (from 0 to 1)
             -> (Int -> v n) -- ^ The window function
             -> v n
windowedSinc size cutoff window = VG.zipWith (*) (sinc size cutoff) (window size)

signal :: [Double] -> [Double] -> [(Double, Double)]
signal coeffs xs = [ (x / pi, func x) | x <- xs ]
    where
    func phase = magnitude $ sum $ zipWith (\index mag -> mkPolar mag (phase * (- index))) (iterate (+ 1) (- ((fromIntegral (length coeffs) - 1) / 2))) coeffs

-- | Given filter coefficients, plot their frequency response and save the graph as a png file
plotFrequency :: [Double] -- ^ The filter coefficients
              -> FilePath -- ^ The filename 
              -> IO ()
plotFrequency coeffs fName = toFile def fName $ do
    layout_title .= "Frequency Response"
    plot (line "Frequency Response" [signal coeffs $ takeWhile (< pi) $ iterate (+ 0.01) 0])

--ts is really the ratio ts / sampling period
-- | Square root raised cosine
srrc :: (Ord a, Floating a) 
     => Int -- ^ size: from [-n .. n]
     -> Int -- ^ sampling period
     -> a   -- ^ beta
     -> [a]
srrc n ts beta = map func [(-n) .. n]
    where
    func x 
        | x == 0                                                 = 1 - beta + 4 * beta / pi
        | abs (fromIntegral x) ~= (fromIntegral ts / (4 * beta)) = (beta / sqrt 2) * ((1 + 2/pi) * sin (pi / (4 * beta)) + (1 - 2/pi) * cos (pi / (4 * beta)))
        | otherwise                                              = (sin (pi * xdivts * (1 - beta)) + 4 * beta * xdivts * cos (pi * xdivts * (1 + beta))) / (pi * xdivts * (1 - (4 * beta * xdivts) ** 2))
            where
            xdivts = fromIntegral x / fromIntegral ts
    x ~= y = abs (x - y) < 0.001

