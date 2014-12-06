{-# LANGUAGE FlexibleContexts #-}
module SDR.Demod where

import Data.Complex
import Data.Vector.Generic
import Data.Vector.Fusion.Stream

import Pipes

import SDR.Util
import qualified SDR.Stream as S

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

{-# INLINE fmDemodStr2 #-}
fmDemodStr2 :: (RealFloat a) => Complex a -> S.Stream (Complex a) () -> S.Stream a (Complex a)
fmDemodStr2 x s = fmap snd $ S.mapAccumV func x s
    where
    func last sample = (sample, phase (sample * conjugate last))

{-# INLINE fmDemodVec2 #-}
fmDemodVec2 :: (Vector v (Complex a), Vector v a, RealFloat a, Monad m) => Int -> Complex a -> Pipe (v (Complex a)) (v a) m ()
fmDemodVec2 samples acc = do
    res <- await
    let (acc', buf) = S.unstream samples $ fmDemodStr2 acc $ S.stream res
    yield buf
    fmDemodVec2 samples acc'

