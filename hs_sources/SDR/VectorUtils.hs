{-# LANGUAGE BangPatterns #-}

{-| Various Vector based utility functions -}
module SDR.VectorUtils (
    mapAccumMV,
    stride,
    fill,
    copyInto,
    vUnfoldr,
    vUnfoldrM
    ) where

import Control.Monad
import Control.Monad.Primitive
import Control.Monad.ST

import qualified Data.Vector.Generic               as VG   
import qualified Data.Vector.Generic.Mutable       as VGM
import qualified Data.Vector.Fusion.Bundle         as VFB  
import           Data.Vector.Fusion.Stream.Monadic as VFSM

{-| Like mapAccumL but monadic and over vectors. Doesn't return the
    accumulator at the end because it doesn't seem to be possible to do
    this with the Stream datatype, making this function pretty useless.
-}
mapAccumMV :: (Monad m) 
           => (acc -> x -> m (acc, y)) -- ^ The function
           -> acc                      -- ^ The initial accumulator
           -> VFSM.Stream m x          -- ^ The input stream
           -> Stream m y               -- ^ The output stream
mapAccumMV func z (VFSM.Stream step s) = VFSM.Stream step' (s, z)
    where
    step' (s, acc) = do
        r <- step s
        case r of
            VFB.Yield y s' -> do
                (!acc', !res) <- func acc y 
                return $ VFB.Yield res (s', acc')
            VFB.Skip    s' -> return $ VFB.Skip (s', acc)
            VFB.Done       -> return VFB.Done

{-| Create a vector from another vector containing only the elements that
    occur every stride elements in the source vector.
-}
{-# INLINE stride #-}
stride :: VG.Vector v a 
       => Int -- ^ The stride
       -> v a -- ^ The input Vector
       -> v a -- ^ The output Vector
stride str inv = VG.unstream $ VFB.unfoldr func 0
    where
    len = VG.length inv
    func i | i >= len  = Nothing
           | otherwise = Just (VG.unsafeIndex inv i, i + str)

-- | Fill a mutable vector from a monadic stream. This appears to be missing from the Vector library.
{-# INLINE fill #-}
fill :: (PrimMonad m, Functor m, VGM.MVector vm a) 
     => VFB.Bundle v a    -- ^ The input Stream
     -> vm (PrimState m) a -- ^ The mutable Vector to stream into
     -> m ()
fill str outBuf = void $ VFB.foldM' put 0 str
    where 
    put i x = do
        VGM.unsafeWrite outBuf i x
        return $ i + 1

-- | Copy a Vector into a mutable vector
{-# INLINE copyInto #-}
copyInto :: (PrimMonad m, VGM.MVector vm a, VG.Vector v a)
         => vm (PrimState m) a -- ^ The destination
         -> v a                -- ^ The source
         -> m ()
copyInto dst src = fill (VG.stream src) dst

-- | Similar to unfoldrN from the vector package but the generator function cannot terminate and it returns the final value of the seed in addition to the vector
{-# INLINE vUnfoldr #-}
vUnfoldr :: VG.Vector v x 
         => Int               -- ^ Generates a vector with this size
         -> (acc -> (x, acc)) -- ^ The generator function
         -> acc               -- ^ The initial value of the seed
         -> (v x, acc)        -- ^ The (vector, final value of seed) result
vUnfoldr size func acc = runST $ do
    vect <- VGM.new size
    acc' <- go vect 0 acc
    vect' <- VG.unsafeFreeze vect
    return (vect', acc')
    where
    go vect offset acc = go' offset acc
        where
        go' offset acc 
            | offset == size = return acc
            | otherwise      = do
                let (res, acc') = func acc
                VGM.write vect offset res
                go' (offset + 1) acc'

-- | The same as `vUnfoldr` but the generator function is monadic
{-# INLINE vUnfoldrM #-}
vUnfoldrM :: (PrimMonad m, VG.Vector v x) 
          => Int                 -- ^ Generates a vector with this size
          -> (acc -> m (x, acc)) -- ^ The monadic generator function                    
          -> acc                 -- ^ The initial value of the seed
          -> m (v x, acc)        -- ^ The (vector, final value of seed) result
vUnfoldrM size func acc = do
    vect <- VGM.new size
    acc' <- go vect 0 acc
    vect' <- VG.unsafeFreeze vect
    return (vect', acc')
    where
    go vect offset acc = go' offset acc
        where
        go' offset acc 
            | offset == size = return acc
            | otherwise      = do
                (res, acc') <- func acc
                VGM.write vect offset res
                go' (offset + 1) acc'

