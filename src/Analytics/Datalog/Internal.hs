{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
--------------------------------------------------------------------
-- |
-- Module    :  Analytics.Datalog.Internal
-- Copyright :  (c) Edward Kmett 2013
-- License   :  BSD3
-- Maintainer:  Edward Kmett <ekmett@gmail.com>
-- Stability :  experimental
-- Portability: non-portable
--
-- This module is not considered packaged under the package versioning
-- policy. Any direct dependency upon it is likely to break even
-- between minor versions.
--------------------------------------------------------------------
module Analytics.Datalog.Internal
  ( Match(..)
  , Variable(..), matchVar
  -- * Datalog
  , Datalog(..), query
  , Relation(..)
  , Body(..), no
  -- * Implementation Details
  , Rel(..)
  ) where

import Control.Applicative
import Control.Lens
import Control.Monad
import Control.Monad.Trans.Class
import Data.Foldable
import Data.Functor.Bind
import Data.Functor.Identity
import Data.Functor.Plus
import Data.Semigroup
import Data.Traversable as T
import Data.Typeable
import Data.Void
import Generics.Deriving
import Prelude.Extras

infixr 1 :-

------------------------------------------------------------------------------
-- Rel
------------------------------------------------------------------------------

-- | This provides overloading of predicates so they can be used directly
-- as facts in the 'Datalog' 'Monad' or as the head or 'Body' of a rule or as a
-- 'query'.
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

------------------------------------------------------------------------------
-- Body
------------------------------------------------------------------------------

-- | This models a strongly typed query or the right hand side of a 'Datalog' rule.
--
-- The 'Body' itself forms an 'Alternative', letting you combine them to make a robust 
-- 'query' language.
data Body v a where
  Ap    :: Body v (a -> b) -> Body v a -> Body v b
  Map   :: (a -> b) -> Body v a -> Body v b
  Pure  :: a -> Body v a
  Alt   :: Body v a -> Body v a -> Body v a
  Empty :: Body v a
  Body  :: (Typeable1 t, Match t) => t v -> Body v (t a)
  No    :: Relation v -> Body v ()
  -- TODO: aggregations

instance Functor (Body v) where
  fmap = Map
  {-# INLINE fmap #-}

instance Apply (Body v) where
  (<.>) = Ap
  {-# INLINE (<.>) #-}

instance Applicative (Body v) where
  pure = Pure
  {-# INLINE pure #-}
  (<*>) = Ap
  {-# INLINE (<*>) #-}

instance Alt (Body v) where
  (<!>) = Alt
  {-# INLINE (<!>) #-}

instance Plus (Body v) where
  zero = Empty
  {-# INLINE zero #-}

instance Alternative (Body v) where
  empty = Empty
  {-# INLINE empty #-}
  (<|>) = Alt
  {-# INLINE (<|>) #-}

instance Semigroup a => Semigroup (Body v a) where
  (<>) = liftA2 (<>)
  {-# INLINE (<>) #-}

instance Monoid a => Monoid (Body v a) where
  mempty = pure mempty
  {-# INLINE mempty #-}
  mappend = liftA2 mappend
  {-# INLINE mappend #-}

-- | Anything you can 'Match' can be used directly as a 'Body', too.
instance (Typeable1 t, Match t, ta ~ t a) => Rel t v (Body v ta) where
  rel ta = Body ta
  {-# INLINE rel #-}

-- | Stratified negation.
no :: Relation a -> Body a ()
no = No
{-# INLINE no #-}

------------------------------------------------------------------------------
-- Datalog
------------------------------------------------------------------------------

-- | An @operational@ encoding of a datalog program.
data Datalog :: (* -> *) -> * -> * where
  Fact   :: (Typeable1 t, Match t) => (forall a. t a) -> Datalog m ()
  (:-)   :: Ord a => Relation a -> Body a t -> Datalog m ()
  Query  :: Ord a => Body a t -> Datalog m [t]
  Bind   :: Datalog m a -> (a -> Datalog m b) -> Datalog m b
  Return :: a -> Datalog m a
  Lift   :: m a -> Datalog m a

instance Functor (Datalog m) where
  fmap = liftM
  {-# INLINE fmap #-}

instance Apply (Datalog m) where
  (<.>) = ap
  {-# INLINE (<.>) #-}

instance Applicative (Datalog m) where
  pure = Return
  {-# INLINE pure #-}
  (<*>) = ap
  {-# INLINE (<*>) #-}

instance Bind (Datalog m) where
  (>>-) = Bind
  {-# INLINE (>>-) #-}

instance Monad (Datalog m) where
  return = Return
  {-# INLINE return #-}
  (>>=) = Bind
  {-# INLINE (>>=) #-}

instance MonadTrans Datalog where
  lift = Lift
  {-# INLINE lift #-}

instance (Typeable1 t, Match t, u ~ ()) => Rel t Void (Datalog m u) where
  rel tv = Fact (vacuous tv)
  {-# INLINE rel #-}

query :: Ord a => Body a t -> Datalog m [t]
query = Query
{-# INLINE query #-}

------------------------------------------------------------------------------
-- Matching
------------------------------------------------------------------------------

-- | Simple flat unification
class Traversable t => Match t where
  -- | Use 'Generic1' by default
  --
  -- @default 'match' :: ('Generic1' t, 'GMatch' ('Rep1' t)) => (a -> b -> c) -> t a -> t b -> 'Maybe' (t c)@
  match :: (a -> b -> c) -> t a -> t b -> Maybe (t c)
  default match :: (Generic1 t, GMatch (Rep1 t)) => (a -> b -> c) -> t a -> t b -> Maybe (t c)
  match abc ta tb = to1 <$> gmatch abc (from1 ta) (from1 tb)
  {-# INLINE match #-}

------------------------------------------------------------------------------
-- Generic Matching
------------------------------------------------------------------------------

-- | Enable matching via 'Generic' programming.
class GMatch t where
  gmatch :: (a -> b -> c) -> t a -> t b -> Maybe (t c)

instance GMatch U1 where
  gmatch _ U1 U1 = Just U1
  {-# INLINE gmatch #-}

instance GMatch Par1 where
  gmatch f (Par1 a) (Par1 b) = Just $ Par1 (f a b)
  {-# INLINE gmatch #-}

instance Eq c => GMatch (K1 i c) where
  gmatch _ (K1 a) (K1 b)
    | a == b    = Just (K1 a)
    | otherwise = Nothing
  {-# INLINE gmatch #-}

instance GMatch f => GMatch (Rec1 f) where
  gmatch f (Rec1 a) (Rec1 b) = Rec1 <$> gmatch f a b
  {-# INLINE gmatch #-}

instance (GMatch f, GMatch g) => GMatch (f :+: g) where
  gmatch f (L1 a) (L1 b) = L1 <$> gmatch f a b
  gmatch f (R1 a) (R1 b) = R1 <$> gmatch f a b
  gmatch _ _      _      = Nothing
  {-# INLINE gmatch #-}

instance (GMatch f, GMatch g) => GMatch (f :*: g) where
  gmatch f (a :*: b) (c :*: d) = (:*:) <$> gmatch f a c <*> gmatch f b d
  {-# INLINE gmatch #-}

instance (Match f, GMatch g) => GMatch (f :.: g) where
  gmatch f (Comp1 x) (Comp1 y) = do
    ms <- match (gmatch f) x y
    Comp1 <$> T.sequence ms
  {-# INLINE gmatch #-}

------------------------------------------------------------------------------
-- Variable
------------------------------------------------------------------------------

-- | A 'Variable' type has a distinguishable 'var' construction.
class (Match v, Eq1 v) => Variable v where
  var :: Prism (v a) (v b) a b

-- | Default definition of 'match' for a 'Var'
matchVar :: Variable t => (a -> b -> c) -> t a -> t b -> Maybe (t c)
matchVar f va vb = case var Left va of
  Left a -> Just (f a <$> vb)
  Right a' -> case var Left vb of
    Left b -> Just ((`f` b) <$> va)
    Right b' | a' ==# b' -> Just (vacuous a')
             | otherwise -> Nothing
