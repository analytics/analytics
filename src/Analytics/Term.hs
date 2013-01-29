{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
--------------------------------------------------------------------
-- |
-- Module    :  Analytics.Term
-- Copyright :  (c) Edward Kmett 2012-2013
-- License   :  BSD3
-- Maintainer:  Edward Kmett <ekmett@gmail.com>
-- Stability :  experimental
-- Portability: non-portable
--
--------------------------------------------------------------------
module Analytics.Term
  (
  -- * Terms
    Term(..)
  , prettyTerm
  , prettyTerm_
  -- * Term Variables
  , HasVars(..)
  ) where

import Analytics.Syntax
import Analytics.Pretty
import Control.Applicative
import Control.Lens
import Data.Text.Lens hiding (text)
import Control.Monad
import Prelude.Extras
import Data.IntMap
import Data.Foldable
import Data.Functor.Identity
import Data.String
import Data.Text
import Data.Traversable
import Data.Data
import Data.Map as Map

-- $setup
-- >>> :set -XOverloadedStrings

------------------------------------------------------------------------------
-- Term
------------------------------------------------------------------------------

-- | Terms with kind variables of type @a@
data Term a
  = Var a
  | Struct Text [Term a]
  deriving (Eq, Ord, Show, Read, Data, Typeable)

instance a ~ String => IsString (Term a) where
  fromString = Var . fromString

instance Variable Term where
  var = prism Var $ \t -> case t of
    Var a -> Right a
    _     -> Left  t

instance Plated (Term a) where
  plate f (Struct h xs) = Struct h <$> traverse f xs
  plate _ v = pure v

instance Functor Term where
  fmap = fmapDefault

instance Foldable Term where
  foldMap = foldMapDefault

instance Traversable Term where
  traverse f (Var a)       = Var <$> f a
  traverse f (Struct h xs) = Struct h <$> traverse (traverse f) xs

instance Applicative Term where
  pure = Var
  (<*>) = ap

instance Monad Term where
  return = Var
  Var a >>= f      = f a
  Struct h xs >>= f = Struct h (Prelude.map (>>= f) xs)

instance Eq1 Term
instance Ord1 Term
instance Show1 Term
instance Read1 Term

------------------------------------------------------------------------------
-- HasVars
------------------------------------------------------------------------------

-- | Pretty print a 'Term', using a helper to print free variables
prettyTerm :: Applicative f => Term a -> (a -> f Doc) -> f Doc
prettyTerm (Var a)       k = k a
prettyTerm (Struct x xs) k = fillSep . (text (x^.unpacked) :) <$> traverse (`prettyTerm` k) xs

prettyTerm_  :: Term String -> Doc
prettyTerm_ k = runIdentity $ prettyTerm k $ Identity . pretty

------------------------------------------------------------------------------
-- HasVars
------------------------------------------------------------------------------

-- | Provides a traversal of free term variables that can be used to perform
-- substitution or extract a free variable set.
class HasVars s t a b | s -> a, s b -> t, t -> b, t a -> s where
  vars :: Traversal s t a b

instance HasVars (Term a) (Term b) a b where
  vars = traverse

instance HasVars s t a b => HasVars [s] [t] a b where
  vars = traverse.vars

instance HasVars s t a b => HasVars (IntMap s) (IntMap t) a b where
  vars = traverse.vars

instance HasVars s t a b => HasVars (Map k s) (Map k t) a b where
  vars = traverse.vars
