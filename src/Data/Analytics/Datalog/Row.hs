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
module Data.Analytics.Datalog.Row
  ( Row(..)
  , arg
  , match
  , ARow
  ) where

import Control.Applicative
import Control.Lens
import Control.Monad
import Control.Monad.State
import Data.Analytics.Datalog.Term
import Data.Analytics.Datalog.Subst
import Data.Maybe
import Data.Monoid
import Data.Typeable

data ARow = forall a. Typeable a => ARow (Row a)
  deriving Typeable

instance Eq ARow where
  ARow a == ARow b = cast a == Just b
  {-# INLINE (==) #-}

instance Ord ARow where
  ARow x `compare` ARow y = compareRow x y
  {-# INLINE compare #-}

------------------------------------------------------------------------------
-- Row

------------------------------------------------------------------------------

-- | A relation with free variables of type @a@. This is used to represent
-- the head of a datalog rule and is used by 'no'.
data Row a where
  RowAp   :: Row (a -> b) -> Row a -> Row b
  RowMap  :: (a -> b) -> Row a -> Row b
  RowPure :: a -> Row a
  RowArg  :: Term a => a -> Row (Entity a)
  deriving Typeable

instance Eq (Row a) where
  a == b = compare a b == EQ
  {-# INLINE (==) #-}

compareRow :: Row a -> Row b -> Ordering
compareRow (RowAp f x)  (RowAp g y)  = compareRow f g <> compareRow x y
compareRow RowAp{}      _            = LT
compareRow RowMap{}     RowAp{}      = GT
compareRow (RowMap _ x) (RowMap _ y) = compareRow x y
compareRow RowMap{}     _            = LT
compareRow RowPure{}    RowPure{}    = EQ
compareRow RowPure{}    RowArg{}     = LT
compareRow RowPure{}    _            = GT
compareRow (RowArg a)   (RowArg b)   = cast a `compare` Just b
compareRow RowArg{}     _            = GT

instance Ord (Row a) where
  compare = compareRow
  {-# INLINE compare #-}

arg :: Term a => a -> Row (Entity a)
arg = RowArg
{-# INLINE arg #-}

instance Functor Row where
  fmap = RowMap
  {-# INLINE fmap #-}

instance Applicative Row where
  pure = RowPure
  {-# INLINE pure #-}
  (<*>) = RowAp
  {-# INLINE (<*>) #-}

instance HasVars (Row a) where
  applyM s (RowAp l r)  = liftM2 RowAp (applyM s l) (applyM s r)
  applyM s (RowMap f x) = liftM (RowMap f) (applyM s x)
  applyM _ (RowPure a)  = return (RowPure a)
  applyM s (RowArg v)   = case s^.mgu.at (Var v) of
    Nothing -> return $ RowArg v
    Just (AVar v')    -> case gcast (RowArg v') of
      Just h  -> return h
      Nothing -> fail "PANIC: illegal substitution"
    Just (AnEntity t) -> case gcast (RowArg t) of
      Just h -> return h
      Nothing -> fail "PANIC: illegal substitution"
  {-# INLINEABLE applyM #-}

  vars f (RowAp l r)  = RowAp <$> vars f l <*> vars f r
  vars f (RowMap k x) = RowMap k <$> vars f x
  vars _ (RowPure a)  = pure (RowPure a)
  vars f (RowArg v)   = f (Var v) <&> \(Var u) ->
    RowArg (fromMaybe (error "PANIC: very illegal substitution" `asTypeOf` v) (cast u))
  {-# INLINEABLE vars #-}

match :: (MonadState s m, HasSubst s, MonadPlus m) => Row a -> Row b -> m (Row a)
match (RowAp l r) (RowAp l' r')   = liftM2 RowAp (match l l') (match r r')
match (RowMap f x) (RowMap _ y)   = RowMap f `liftM` match x y
match (RowPure a) RowPure{}       = return (RowPure a)
match l@(RowArg t) r@(RowArg t')  = case term `withArgType` t of
  IsVar -> do
    u <- use subst
    case u^.mgu.at (Var t) of
      Just (AVar t'') -> do
         h <- match (RowArg t'') r
         maybe (error "broken heart") return $ cast h
      Just (AnEntity t'') -> do
         h <- match (RowArg t'') r
         maybe (error "broken heart") return $ cast h
      Nothing -> do
         subst .= apply (t ~> t') u
         return l
  IsEntity -> case term `withArgType` t' of
    IsVar -> do
      u <- use subst
      case u^.mgu.at (Var t') of
        Just (AVar t'')     -> match l (RowArg t'')
        Just (AnEntity t'') -> match l (RowArg t'')
        Nothing -> do
          subst .= apply (t' ~> t) u
          return l
    IsEntity
       | cast t == Just t' -> return l
       | otherwise         -> fail "unification"
match _ _ = error "broken heart"
{-# INLINEABLE match #-}

withArgType :: t a -> a -> t a
withArgType = const
{-# INLINE withArgType #-}
