{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE GADTs #-}
--------------------------------------------------------------------
-- |
-- Module    :  Data.Analytics.Datalog.Atom
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
import Data.Analytics.Datalog.Term
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

arg :: Term a => a -> Heart (Entity a)
arg = ArgH

instance Functor Heart where
  fmap = MapH

instance Applicative Heart where
  pure = PureH
  (<*>) = ApH

{-
withArgType :: t a -> a -> t a
withArgType = const

match :: Heart a -> Heart b -> Unified (Heart a)
match (ApH l r) (ApH l' r') = ApH <$> match l l' <*> match r r'
match (MapH f x) (MapH _ y) = MapH f <$> match x y
match (PureH a) (PureH _)   = pure (PureH a)
match l@(ArgH t) (ArgH t')  = case term `withArgType` t of
  IsVariable -> case term `withArgType` t' of
    IsVariable -> Just l

  term `withArgType t'
match _ _ = Nothing
-}
