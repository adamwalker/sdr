{-# LANGUAGE BangPatterns #-}

{-| Various Vector based utility functions -}
module SDR.VectorUtils (
    mapAccumMV,
    stride,
    fill,
    vUnfoldr,
    vUnfoldrM
    ) where

import Control.Monad
import Control.Monad.Primitive
import Control.Monad.ST

import           Data.Vector.Generic               as VG   hiding ((++))
import qualified Data.Vector.Generic.Mutable       as VGM
import           Data.Vector.Fusion.Stream.Monadic         hiding ((++))
import qualified Data.Vector.Fusion.Stream         as VFS  hiding ((++))
import qualified Data.Vector.Fusion.Stream.Monadic as VFSM hiding ((++))

{-| Like mapAccumL but monadic and over vectors. Doesn't return the
    accumulator at the end because it doesn't seem to be possible to do
    this with the Stream datatype, making this function pretty useless.
-}
mapAccumMV :: (Monad m) 
           => (acc -> x -> m (acc, y)) -- ^ The function
           -> acc                      -- ^ The initial accumulator
           -> Stream m x               -- ^ The input stream
           -> Stream m y               -- ^ The output stream
mapAccumMV func z (Stream step s sz) = Stream step' (s, z) sz
    where
    step' (s, acc) = do
        r <- step s
        case r of
            Yield y s' -> do
                (!acc', !res) <- func acc y 
                return $ Yield res (s', acc')
            Skip    s' -> return $ Skip (s', acc)
            Done       -> return Done

{-| Create a vector from another vector containing only the elements that
    occur every stride elements in the source vector.
-}
{-# INLINE stride #-}
stride :: VG.Vector v a 
       => Int -- ^ The stride
       -> v a -- ^ The input Vector
       -> v a -- ^ The output Vector
stride str inv = VG.unstream $ VFS.unfoldr func 0
    where
    len = VG.length inv
    func i | i >= len  = Nothing
           | otherwise = Just (VG.unsafeIndex inv i, i + str)

-- | Fill a mutable vector from a monadic stream
{-# INLINE fill #-}
fill :: (PrimMonad m, Functor m, VGM.MVector vm a) 
     => VFS.MStream m a    -- ^ The input Stream
     -> vm (PrimState m) a -- ^ The mutable Vector to stream into
     -> m ()
fill str outBuf = void $ VFSM.foldM' put 0 str
    where 
    put i x = do
        VGM.unsafeWrite outBuf i x
        return $ i + 1

vUnfoldr :: VG.Vector v x => Int -> (acc -> (x, acc)) -> acc -> (v x, acc)
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

vUnfoldrM :: (PrimMonad m, VG.Vector v x) => Int -> (acc -> m (x, acc)) -> acc -> m (v x, acc)
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

