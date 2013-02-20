{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE GADTs #-}
--------------------------------------------------------------------
-- |
-- Copyright :  (c) Edward Kmett 2013
-- License   :  BSD3
-- Maintainer:  Edward Kmett <ekmett@gmail.com>
-- Stability :  experimental
-- Portability: non-portable
--
--------------------------------------------------------------------
module Data.Analytics.Datalog.Atom
  ( Atom(..)
  , Heart(..)
  , arg
  ) where

import Control.Applicative
import Control.Lens
import Control.Monad
import Data.Analytics.Datalog.Term
import Data.Analytics.Datalog.Subst
import Data.Maybe
import Data.Monoid
import Data.Typeable


data Atom :: * -> * -> * where
  Atom :: (b -> a) -> t -> Heart b -> Atom t a
  deriving Typeable

instance Functor (Atom t) where
  fmap f (Atom k t h) = Atom (f . k) t h

instance Term x => TermOf (Atom t a) x

------------------------------------------------------------------------------
-- Heart
------------------------------------------------------------------------------

-- | A relation with free variables of type @a@. This is used to represent
-- the head of a datalog rule and is used by 'no'.
data Heart a where
  ApH   :: Heart (a -> b) -> Heart a -> Heart b
  MapH  :: (a -> b) -> Heart a -> Heart b
  PureH :: a -> Heart a
  ArgH  :: Term a => a -> Heart (Entity a)
  deriving Typeable

instance Eq (Heart a) where
  a == b = compare a b == EQ
  {-# INLINE (==) #-}

compareHeart :: Heart a -> Heart b -> Ordering
compareHeart (ApH f x)  (ApH g y)  = compareHeart f g <> compareHeart x y
compareHeart ApH{}      _          = LT
compareHeart MapH{}     ApH{}      = GT
compareHeart (MapH _ x) (MapH _ y) = compareHeart x y
compareHeart MapH{}     _          = LT
compareHeart PureH{}    PureH{}    = EQ
compareHeart PureH{}    ArgH{}     = LT
compareHeart PureH{}    _          = GT
compareHeart (ArgH a)   (ArgH b)   = cast a `compare` Just b
compareHeart ArgH{}     _          = GT

instance Ord (Heart a) where
  compare = compareHeart
  {-# INLINE compare #-}

arg :: Term a => a -> Heart (Entity a)
arg = ArgH
{-# INLINE arg #-}

instance Functor Heart where
  fmap = MapH
  {-# INLINE fmap #-}

instance Applicative Heart where
  pure = PureH
  {-# INLINE pure #-}
  (<*>) = ApH
  {-# INLINE (<*>) #-}

instance HasVars (Heart a) where
  applyM s (ApH l r)  = liftM2 ApH (applyM s l) (applyM s r)
  applyM s (MapH f x) = liftM (MapH f) (applyM s x)
  applyM _ (PureH a)  = return (PureH a)
  applyM s (ArgH v)   = case s^.mgu.at (Var v) of
    Nothing -> return $ ArgH v
    Just (AVar v')    -> case gcast (ArgH v') of
      Just h  -> return h
      Nothing -> fail "PANIC: illegal substitution"
    Just (AnEntity t) -> case gcast (ArgH t) of
      Just h -> return h
      Nothing -> fail "PANIC: illegal substitution"
  {-# INLINEABLE applyM #-}

  vars f (ApH l r) = ApH <$> vars f l <*> vars f r
  vars f (MapH k x) = MapH k <$> vars f x
  vars _ (PureH a) = pure (PureH a)
  vars f (ArgH v) = f (Var v) <&> \(Var u) ->
    ArgH (fromMaybe (error "PANIC: very illegal substitution" `asTypeOf` v) (cast u))
  {-# INLINEABLE vars #-}

{-
match :: (MonadState s m, HasSubst s, MonadPlus m) => Heart a -> Heart b -> m (Heart a)
match (ApH l r) (ApH l' r') = ApH <$> match l l' <*> match r r'
match (MapH f x) (MapH _ y) = MapH f <$> match x y
match (PureH a) (PureH _)   = pure (PureH a)
match l@(ArgH t) r@(ArgH t')  = case term `withArgType` t of
  IsVar -> use subst >>= \u -> case u^.mgu.at (Var t) of
    Just t'' -> match (ArgH t'') r
    Nothing  -> do
      subst .= apply (t ~> t') u

withArgType :: t a -> a -> t a
withArgType = const
-}
