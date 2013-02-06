{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
--------------------------------------------------------------------
-- |
-- Module    :  Data.Analytics.Datalog.Subst
-- Copyright :  (c) Edward Kmett 2013
-- License   :  BSD3
-- Maintainer:  Edward Kmett <ekmett@gmail.com>
-- Stability :  experimental
-- Portability: non-portable
--
--------------------------------------------------------------------
module Data.Analytics.Datalog.Subst
  ( Var(..)
  , ATerm(..)
  , HasVars(..)
  , Subst(..)
  , HasSubst(..)
  , apply
  ) where

import Control.Applicative
import Control.Lens
import Control.Monad
import Control.Monad.Writer hiding ((<>))
import Data.Analytics.Datalog.Term
import Data.HashMap.Lazy
import Data.IntMap
import Data.Map
import Data.Maybe
import Data.Semigroup
import Data.Traversable as Traversable
import Data.Typeable
import Unsafe.Coerce
import Prelude

data Var where
  Var :: Term t => t -> Var

instance Eq Var where
  Var x == Var y = maybe False (==y) (cast x)
  {-# INLINE (==) #-}

instance Ord Var where
  Var x `compare` Var y = case typeOf x `compare` typeOf y of
    LT -> LT
    EQ -> unsafeCoerce x `compare` y
    GT -> GT
  {-# INLINE compare #-}

data ATerm where
  AVar     :: Term t => t -> ATerm
  AnEntity :: (Term t, t ~ Entity t) => t -> ATerm

newtype Subst = Subst { _mgu :: Map Var ATerm }

makeClassy ''Subst

class HasVars t where
  applyM :: MonadWriter Any m => Subst -> t -> m t
  vars :: Traversal' t Var

applyN :: (HasVars t, MonadWriter Any m) => Subst -> t -> m t
applyN s t = do
  (t', Any b) <- listen (applyM s t)
  return $ if b then t' else t
{-# INLINE applyN #-}

apply :: HasVars t => Subst -> t -> t
apply s t = case runWriter (applyM s t) of
  (_, Any False) -> t
  (t', Any True) -> t'
{-# INLINE apply #-}

instance HasVars ATerm where
  applyM (Subst l) t@(AVar v) = case l^.at (Var v) of
    Just r  -> do
      tell (Any True)
      return r
    Nothing -> return t
  applyM _         t = return t
  {-# INLINE applyM #-}

  vars f (AVar v) = (\(Var v') -> AVar v') <$> f (Var v)
  vars _ s = pure s
  {-# INLINE vars #-}

instance HasVars t => HasVars [t] where
  applyM = Traversable.mapM.applyN
  {-# INLINE applyM #-}
  vars = traverse.vars
  {-# INLINE vars #-}

instance HasVars t => HasVars (Map k t) where
  applyM = Traversable.mapM.applyN
  {-# INLINE applyM #-}
  vars = traverse.vars
  {-# INLINE vars #-}

instance HasVars t => HasVars (HashMap k t) where
  applyM = Traversable.mapM.applyN
  {-# INLINE applyM #-}
  vars = traverse.vars
  {-# INLINE vars #-}

instance HasVars t => HasVars (IntMap t) where
  applyM = Traversable.mapM.applyN
  {-# INLINE applyM #-}
  vars = traverse.vars
  {-# INLINE vars #-}

instance HasVars Subst where
  applyM l (Subst r) = Subst `liftM` applyM l r
  {-# INLINE applyM #-}
  vars f (Subst r) = Subst <$> vars f r
  {-# INLINE vars #-}

instance Semigroup Subst where
  s@(Subst l) <> Subst m = Subst (apply s m <> l)
  {-# INLINE (<>) #-}

instance Monoid Subst where
  mempty = Subst mempty
  {-# INLINE mempty #-}
  mappend = (<>)
  {-# INLINE mappend #-}
