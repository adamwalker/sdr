{-# LANGUAGE FlexibleContexts #-}
module SDR.Demod (
    fmDemodStr,
    fmDemodVec
    ) where

import Data.Complex
import Data.Vector.Generic
import Data.Vector.Fusion.Stream

import Pipes

import SDR.Util

-- | FM demodulate a stream of complex samples
{-# INLINE fmDemodStr #-}
fmDemodStr :: (RealFloat a) => Complex a -> Stream (Complex a) -> Stream a
fmDemodStr = mapAccumMV func 
    where
    {-# INLINE func #-}
    func last sample = return (sample, phase (sample * conjugate last))

-- | FM demodulate a vector of complex samples
{-# INLINE fmDemodVec #-}
fmDemodVec :: (RealFloat a, Vector v (Complex a), Vector v a) => Complex a -> v (Complex a) -> v a
fmDemodVec init = unstream . fmDemodStr init . stream

