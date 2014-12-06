{-# LANGUAGE RankNTypes, BangPatterns, ExistentialQuantification, DeriveFunctor #-}

module SDR.Stream where

import Data.Vector.Generic as V
import Data.Vector.Fusion.Util 
import qualified Data.Vector.Generic.Mutable       as VGM
import Control.Monad.Primitive
import Control.Monad.ST

data Step s a r = Yield a s
                | Skip    s
                | Done  r
                deriving (Functor)

data Stream a r = forall s. Stream (s -> Step s a r) s

instance Functor (Stream a) where
    fmap f (Stream func s) = Stream ((fmap . fmap) f func) s

{-# INLINE map #-}
map :: (a -> b) -> Stream a r -> Stream b r
map f (Stream step s) = Stream step' s
    where
    {-# INLINE step' #-}
    step' s = case step s of
        Yield y s' -> let res = f y in Yield res s'
        Skip    s' -> Skip s'
        Done  r    -> Done r

{-# INLINE mapAccumV #-}
mapAccumV :: (acc -> x -> (acc, y)) -> acc -> Stream x r -> Stream y (r, acc)
mapAccumV func i (Stream step s) = Stream step' (s, i)
    where 
    {-# INLINE step' #-}
    step' (s, acc) = case step s of
        Yield y s' -> let (!acc', !res) = func acc y in Yield res (s', acc')
        Skip    s' -> Skip (s', acc)
        Done  r    -> Done (r, acc)

{-# INLINE unfoldr' #-}
unfoldr' :: (s -> Either r (a, s)) -> s -> Stream a r
unfoldr' f s = Stream step s
    where
    {-# INLINE step #-}
    step s = case f s of 
        Right (x, s') -> Yield x s'
        Left  r       -> Done r

{-# INLINE stream #-}
stream :: Vector v a => v a -> Stream a ()
stream v = v `seq` n `seq` unfoldr' get 0 
    where
    n = V.length v
    {-# INLINE get #-}
    get i | i >= n    = Left ()
          | otherwise = case basicUnsafeIndexM v i of Box x -> Right (x, i+1)

{-# INLINE foldMS #-}
foldMS :: Monad m => (a -> b -> m a) -> a -> Stream b r -> m (a, r)
foldMS f acc (Stream step s) = go acc s
    where 
    go acc s = acc `seq` case step s of
        Yield x s' -> do
            acc' <- f acc x
            go acc' s'
        Skip    s' -> go acc s'
        Done  r    -> return (acc, r)

{-# INLINE fill #-}
fill :: (Functor m, PrimMonad m, VGM.MVector vm a) => Stream a r -> vm (PrimState m) a -> m r
fill str outBuf = fmap snd $ foldMS put 0 str
    where
    put i x = VGM.unsafeWrite outBuf i x >> return (i + 1)

{-# INLINE unstream #-}
unstream :: Vector v a => Int -> Stream a r -> (r, v a)
unstream n str = runST $ do
    v <- VGM.unsafeNew n
    r <- fill str v
    v <- V.unsafeFreeze v
    return (r, v)

