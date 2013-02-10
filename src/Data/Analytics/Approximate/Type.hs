{-# LANGUAGE CPP #-}
{-# LANGUAGE TemplateHaskell #-}
module Data.Analytics.Approximate.Type
  ( Approximate(..)
  , exact
  , zero
  , one
  ) where

import Control.Applicative
import Control.Lens
import Data.Copointed
import Data.Foldable
import Data.Functor.Apply
import Data.Monoid
import Data.Pointed

-- | An approximate number, with a likely interval, a selected mode and a lower bound on the probability that the answer falls in the interval.
data Approximate a = Approximate
  { _confidence :: {-# UNPACK #-} !Double
  , _lo, _estimate, _hi :: a
  }

makeLenses ''Approximate

instance Functor Approximate where
  fmap f (Approximate p l m h) = Approximate p (f l) (f m) (f h)
  {-# INLINE fmap #-}

instance Foldable Approximate where
  foldMap f (Approximate _ l m h) = f l `mappend` f m `mappend` f h
  {-# INLINE foldMap #-}

instance Traversable Approximate where
  traverse f (Approximate p l m h) = Approximate p <$> f l <*> f m <*> f h
  {-# INLINE traverse #-}

instance Copointed Approximate where
  copoint (Approximate _ _ a _) = a
  {-# INLINE copoint #-}

instance Pointed Approximate where
  point a = Approximate 1.0 a a a
  {-# INLINE point #-}

instance Apply Approximate where
  Approximate p lf mf hf <.> Approximate q la ma ha = Approximate (p * q) (lf la) (mf ma) (hf ha)
  {-# INLINE (<.>) #-}

instance Applicative Approximate where
  pure a = Approximate 1 a a a
  {-# INLINE pure #-}
  Approximate p lf mf hf <*> Approximate q la ma ha = Approximate (p * q) (lf la) (mf ma) (hf ha)
  {-# INLINE (<*>) #-}

instance (Ord a, Num a) => Num (Approximate a) where
  (+) = liftA2 (+)
  {-# INLINE (+) #-}
  m * n
    | is zero n || is one m = m
    | is zero m || is one n = n
    | otherwise = Approximate (m^.confidence * n^.confidence) (Prelude.minimum extrema) (m^.estimate * n^.estimate) (Prelude.maximum extrema) where
      extrema = (*) <$> [m^.lo,m^.hi] <*> [n^.lo,n^.hi]
  {-# INLINE (*) #-}
  negate (Approximate p l m h) = Approximate p (-h) (-m) (-l)
  {-# INLINE negate #-}
  Approximate p la ma ha - Approximate q lb mb hb = Approximate (p * q) (la - hb) (ma - mb) (ha - lb)
  {-# INLINE (-) #-}
  abs (Approximate p la ma ha) = Approximate p (min lb hb) (abs ma) (max lb hb) where
    lb = abs la
    hb = abs ha
  {-# INLINE abs #-}
  signum = fmap signum
  {-# INLINE signum #-}
  fromInteger = pure . fromInteger
  {-# INLINE fromInteger #-}

exact :: Eq a => Prism' (Approximate a) a
exact = prism pure $ \ s -> case s of
  Approximate p a b c | p == 1.0 && a == c -> Right b
  _ -> Left s
{-# INLINE exact #-}

zero :: (Num a, Eq a) => Prism' (Approximate a) ()
zero = exact.only 0
{-# INLINE zero #-}

one :: (Num a, Eq a) => Prism' (Approximate a) ()
one = exact.only 1
{-# INLINE one #-}

is :: Getting Any s t a b -> s -> Bool
is = has
{-# INLINE is #-}

#if !(MIN_VERSION_lens(3,9,0))
only :: Eq a => a -> Prism' a ()
only a = prism' (\() -> a) $ guard . (a ==)
#endif
