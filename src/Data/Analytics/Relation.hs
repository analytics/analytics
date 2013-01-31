{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
--------------------------------------------------------------------
-- |
-- Module    :  Data.Analytics.Relation
-- Copyright :  (c) Edward Kmett 2013
-- License   :  BSD3
-- Maintainer:  Edward Kmett <ekmett@gmail.com>
-- Stability :  experimental
-- Portability: non-portable
--
--------------------------------------------------------------------
module Data.Analytics.Relation
  ( Relation(..)
  , Rel(..)
  ) where

import Control.Applicative
import Control.Lens
import Data.Analytics.Match
import Data.Foldable
import Data.Functor.Identity
import Data.Typeable

------------------------------------------------------------------------------
-- Rel
------------------------------------------------------------------------------

-- | This provides overloading of predicates so they can be used directly
-- as facts in the 'Data.Analytics.Datalog.Datalog' 'Monad' or as the head or body of a rule or as a
-- 'Data.Analytics.Query.Query'.
class (Typeable1 t, Match t) => Rel t a r | r -> a where
  rel :: t a -> r

------------------------------------------------------------------------------
-- Relation
------------------------------------------------------------------------------

-- | A relation with free variables of type @a@. This is used to represent
-- the head of a datalog rule and is used by 'no'.
data Relation v = forall t. (Typeable1 t, Match t) => Relation (t v)
  deriving Typeable

instance Functor Relation where
  fmap f (Relation tv) = Relation (fmap f tv)
  {-# INLINE fmap #-}

instance Foldable Relation where
  foldMap f (Relation tv) = foldMap f tv
  {-# INLINE foldMap #-}

instance Traversable Relation where
  traverse f (Relation tv) = Relation <$> traverse f tv
  {-# INLINE traverse #-}

cast1 :: (Typeable1 t, Typeable1 t') => t a -> Maybe (t' a)
cast1 = fmap runIdentity . gcast1 . Identity
{-# INLINE cast1 #-}

instance Match Relation where
  match f (Relation x) (Relation y) = do
    y' <- cast1 y
    Relation <$> match f x y'
  {-# INLINE match #-}

instance (Typeable1 t, Match t) => Rel t v (Relation v) where
  rel ta = Relation ta
  {-# INLINE rel #-}
