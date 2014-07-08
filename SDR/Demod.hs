{-# LANGUAGE FlexibleContexts #-}
module SDR.Demod where

import Data.Complex
import Data.Vector.Generic
import Data.Vector.Fusion.Stream

import Pipes

import SDR.Util

{-# INLINE fmDemodStr #-}
fmDemodStr :: (RealFloat a) => Complex a -> Stream (Complex a) -> Stream a
fmDemodStr = mapAccumMV func 
    where
    {-# INLINE func #-}
    func last sample = return (sample, phase (sample * conjugate last))

{-# INLINE fmDemodVec #-}
fmDemodVec :: (RealFloat a, Vector v (Complex a), Vector v a) => Complex a -> v (Complex a) -> v a
fmDemodVec init = unstream . fmDemodStr init . stream

