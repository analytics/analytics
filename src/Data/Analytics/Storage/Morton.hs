{-# LANGUAGE BangPatterns, MultiParamTypeClasses, FlexibleInstances, PatternGuards, StandaloneDeriving, FlexibleContexts, UndecidableInstances #-}
module Data.Analytics.Storage.Morton
  ( Morton
  , fromList
  , toList
  ) where

import Control.Applicative
import Control.Lens
import Control.Lens.Internal.Deque hiding (fromList)
import Data.Bits
import Data.Int
import Data.List (unfoldr)
import Data.Semigroup

-- TODO: we should be able to answer how many bits there are as well, and extract aligned nybbles.

newtype Morton f = Morton (Deque (Pin f))

deriving instance Show (f Int64) => Show (Morton f)

data Pin f = Pin {-# UNPACK #-} !Int (f Int64)

deriving instance Show (f Int64) => Show (Pin f)

instance (Choice p, Applicative f, Functor g, Functor h) => Cons p f (Morton g) (Morton g) (Morton h) (Morton h) (g Bool) (h Bool) where
  _Cons = prism (\(fx, Morton xs) -> Morton $ Pin 1 (fx <&> \x -> if x then 1 else 0) <| xs) $ \(Morton xs) -> case uncons xs of
    Just (Pin n fb, ys) | !m <- n - 1 -> Right $! ((,) $! fmap (`testBit` m) fb) $! Morton (if m <= 0 then ys else Pin m (fmap (`clearBit` m) fb) <| ys)
    Nothing                           -> Left $! Morton mempty

instance (Choice p, Applicative f, Functor g, Functor h) => Snoc p f (Morton g) (Morton g) (Morton h) (Morton h) (g Bool) (h Bool) where
  _Snoc = prism (\(Morton xs, fx) -> Morton $ xs |> Pin 1 (fx <&> \x -> if x then 1 else 0)) $ \(Morton xs) -> case unsnoc xs of
    Just (ys, Pin n fb) -> Right $! ((,) $! Morton $ if n <= 1 then ys else ys |> Pin (n - 1) (fmap (`shiftR` 1) fb)) $! fmap (`testBit` 0) fb
    Nothing             -> Left $! Morton mempty

instance Semigroup (Morton f) where
  (<>) = mappend
  {-# INLINE (<>) #-}

instance Monoid (Morton f) where
  mempty = Morton mempty
  {-# INLINE mempty #-}
  mappend (Morton xs) (Morton ys) = Morton (mappend xs ys)
  {-# INLINE mappend #-}

toList :: Functor f => (Morton f) -> [f Bool]
toList = unfoldr uncons
{-# INLINE toList #-}

fromList :: Functor f => [f Bool] -> Morton f
fromList = foldr cons mempty
{-# INLINE fromList #-}
