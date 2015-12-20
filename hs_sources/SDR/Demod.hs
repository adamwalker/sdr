{-# LANGUAGE FlexibleContexts #-}

{-| FM demodulation pipes -}
module SDR.Demod (
    fmDemodStr,
    fmDemodVec,
    fmDemod
    ) where

import Data.Complex
import qualified Data.Vector.Generic               as VG
import qualified Data.Vector.Fusion.Stream.Monadic as VFSM
import qualified Data.Vector.Fusion.Bundle.Monadic as VFBM
import qualified Data.Vector.Fusion.Bundle.Size    as VFBS

import SDR.VectorUtils
import Pipes

-- | FM demodulate a stream of complex samples
{-# INLINE fmDemodStr #-}
fmDemodStr :: (RealFloat a, Monad m) 
           => Complex a          -- ^ The starting sample - i.e. the last sample in the last buffer
           -> VFSM.Stream m (Complex a) -- ^ The input stream
           -> VFSM.Stream m a           -- ^ The output stream
fmDemodStr = mapAccumMV func 
    where
    {-# INLINE func #-}
    func last sample = return (sample, phase (sample * conjugate last))

-- | FM demodulate a vector of complex samples
{-# INLINE fmDemodVec #-}
fmDemodVec :: (RealFloat a, VG.Vector v (Complex a), VG.Vector v a) 
           => Complex a     -- ^ The starting sample - i.e. the last sample in the last buffer
           -> v (Complex a) -- ^ The input Vector
           -> v a           -- ^ The output Vector
fmDemodVec init inp = VG.unstream $ flip VFBM.fromStream (VFBS.Exact $ VG.length inp) $ fmDemodStr init $ VFBM.elements $ VG.stream inp

-- | Pipe that performs FM demodulation
{-# INLINE fmDemod #-}
fmDemod :: (RealFloat a, VG.Vector v (Complex a), VG.Vector v a) => Pipe (v (Complex a)) (v a) IO ()
fmDemod = func 0
    where
    func lastSample = do
        dat <- await
        yield $ fmDemodVec lastSample dat
        func $ VG.unsafeIndex dat (VG.length dat - 1)
