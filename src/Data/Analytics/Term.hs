{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE Rank2Types #-}
-- {-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
-- {-# LANGUAGE Rank2Types #-}
-- {-# LANGUAGE UndecidableInstances #-}
--------------------------------------------------------------------
-- |
-- Module    :  Data.Analytics.Term
-- Copyright :  (c) Edward Kmett 2013
-- License   :  BSD3
-- Maintainer:  Edward Kmett <ekmett@gmail.com>
-- Stability :  experimental
-- Portability: non-portable
--
--------------------------------------------------------------------
module Data.Analytics.Term
  ( Term(..)
  , _Term
  ) where

import Control.Arrow
import Control.Applicative
import Control.Lens
import Data.Analytics.Internal.Typeable
import Data.Analytics.Match
import Data.Analytics.Variable
import Data.Foldable
import Data.Typeable
import Prelude.Extras

--------------------------------------------------------------------
-- Term
--------------------------------------------------------------------

-- | In Datalog parlance, a 'Term' is Either a 'Variable' or an entity.
data Term a = forall f. (Typeable1 f, Variable f) => Term (f a)
  deriving Typeable

instance Eq1 Term

instance Eq a => Eq (Term a) where
  Term fa == Term fb = maybe False (==# fb) (cast1 fa)
  {-# INLINE (==) #-}

instance Functor Term where
  fmap f (Term s) = Term (fmap f s)

instance Foldable Term where
  foldMap f (Term s) = foldMap f s

instance Traversable Term where
  traverse f (Term s) = Term <$> traverse f s

instance Match Term where
  match f (Term fa) (Term fb) = do
    fa' <- cast1 fa
    Term <$> match f fa' fb
  {-# INLINE match #-}

instance Variable Term where
  var = rmap (fmap Term) . prism bt (\(Term s) -> left Term (seta s)) where
    bt :: (Typeable1 f, Variable f) => a -> f a
    bt a = var # a
    seta :: (Typeable1 f, Variable f) => f a -> Either (f b) a
    seta s = either Right Left (var Left s)
  {-# INLINE var #-}

instance HasVars (Term a) (Term b) a b where
  vars = var
  {-# INLINE vars #-}

_Term :: (Typeable1 f, Variable f) => Prism' (Term a) (f a)
_Term = prism' Term $ \(Term fa) -> cast1 fa
{-# INLINE _Term #-}
