{-# LANGUAGE FlexibleContexts #-}

{-| FM demodulation pipes -}
module SDR.Demod (
    fmDemodStr,
    fmDemodVec
    ) where

import Data.Complex
import Data.Vector.Generic
import Data.Vector.Fusion.Stream

import SDR.VectorUtils

-- | FM demodulate a stream of complex samples
{-# INLINE fmDemodStr #-}
fmDemodStr :: (RealFloat a) 
           => Complex a          -- ^ The starting sample - i.e. the last sample in the last buffer
           -> Stream (Complex a) -- ^ The input stream
           -> Stream a           -- ^ The output stream
fmDemodStr = mapAccumMV func 
    where
    {-# INLINE func #-}
    func last sample = return (sample, phase (sample * conjugate last))

-- | FM demodulate a vector of complex samples
{-# INLINE fmDemodVec #-}
fmDemodVec :: (RealFloat a, Vector v (Complex a), Vector v a) 
           => Complex a     -- ^ The starting sample - i.e. the last sample in the last buffer
           -> v (Complex a) -- ^ The input Vector
           -> v a           -- ^ The output Vector
fmDemodVec init = unstream . fmDemodStr init . stream

