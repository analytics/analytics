{-# LANGUAGE CPP #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
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
  ( Atomic(..)
  , Table(..)
{-
  , Atom(..)
  , Table(..)
  , tableId
  , tableRollup
-}
  ) where

import Control.Applicative
import Control.Monad
import Control.Lens
import Data.Analytics.Datalog.Row
import Data.Analytics.Datalog.Subst
import Data.Analytics.Datalog.Term
import Data.String
import Data.Typeable

data Table a where
  Auto     :: Typeable a => Table a
  Explicit :: Text -> Table a
  Local    :: {-# UNPACK #-} !Int -> Table a

_Auto :: Typeable a => Prism' (Table a) ()
_Auto = prism Auto seta where
  seta Auto = Right ()
  seta s = Left s
{-# INLINE _Auto #-}

_Explicit :: Typeable a => Prism' (Table a) Text
_Explicit = prism Explicit seta where
  seta (Explicit t) = Right t
  seta s = Left s
{-# INLINE _Explicit #-}

_Local :: Typeable a => Prism' (Table a) Int
_Local = prism Local seta where
  seta (Local i) = Right i
  seta s = Left s
{-# INLINE _Local #-}

instance Show (Table a) where
  showsPrec d Auto = showParen (d > 10) $
    showString "Auto {- " . shows (typeOf (undefined :: a)) . showString " -}"
  showsPrec d (Explicit s) = showParen (d > 10) $
    showString "Explicit " . showsPrec 11 s
  showsPrec d (Local i) = showParen (d > 10) $
    showString "Local " . showsPrec 11 i

instance IsString Table where
  fromString = Explicit . Text.pack
  {-# INLINE fromString #-}

class Applicative (Content g) => Atomic v (g :: * -> *) | g -> v where
  atom :: Typeable a => Table a -> Row v a -> g a

data Atom v a = Atom (Table a) (Row v a) deriving Show

instance Show (v a) => Show (Atom v a) where

instance Atomic v (Atom v) where
  atom = Atom
  {-# INLINE atom #-}

data Row v a where
  RowAp   :: !(Row v (a -> b)) -> !(Row v a) -> Row v b
  RowMap  :: (a -> b) -> !(Row v a) -> Row v b
  RowPure :: a -> Row v a
  RowKey  :: (Typeable a, Show a, Ord a) => Schedule a -> Term v a -> Row v a
  RowVal  :: (Typeable a, Show a, Aggregate a) => Term v a -> Row v a
  deriving Typeable

key  :: Typeable a => Schedule a  -> Term v a -> Row v a
key = RowKey

val  :: (Typeable a, Aggregate a) => Term v a -> Row v a
val = RowVal

{-
instance Show (Atom a b) where
  showsPrec d (Atom (Table i _) r) = showParen (d > 10) $
    showsPrec 10 i . showChar ' ' . showsPrec 10 r

instance HasVars (Atom a b) where
  applyM s (Atom t r) = Atom t `liftM` applyM s r
  {-# INLINEABLE applyM #-}

  vars f (Atom l r) = Atom l <$> vars f r
  {-# INLINEABLE vars #-}

instance Term x => TermOf (Atom a b) x

class Atomic r a b where
  atom :: Atom a b -> r

instance (Typeable c, Show c, Typeable d, a ~ c, b ~ d) => Atomic (Atom a b) c d where
  atom = id

-}

{-# LANGUAGE CPP #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE ExistentialQuantification #-}
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
  , runRow
  , arg
  , match
  , matches
  , ARow(..)
  ) where

import Control.Applicative
import Control.Lens
import Control.Monad
import Control.Monad.State
import Control.Monad.Writer
import Data.Analytics.Datalog.Term
import Data.Analytics.Datalog.Subst
import Data.Maybe
import Data.Typeable

data ARow = forall a. Typeable a => ARow !(Row a)
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
#ifndef HLINT
data Row a where
  RowAp   :: !(Row (a -> b)) -> !(Row a) -> Row b
  RowMap  :: (a -> b) -> !(Row a) -> Row b
  RowPure :: a -> Row a
  RowArg  :: Term a => a -> Row (Entity a)
  deriving Typeable
#endif

instance Show (Row a) where
  showsPrec d (RowAp l r) = showParen (d > 10) $
    showsPrec 10 l . showChar ' ' . showsPrec 11 r
  showsPrec d (RowMap _ x) = showParen (d > 10) $
    showString "_ " . showsPrec 11 x
  showsPrec _ RowPure{} = showString "_"
  showsPrec d (RowArg a) = showsPrec d a

runRow :: Row a -> Maybe a
runRow (RowAp l r)  = runRow l <*> runRow r
runRow (RowMap f x) = f <$> runRow x
runRow (RowPure a)  = Just a
runRow (RowArg a) = case term `withArgType` a of
  IsEntity -> Just a
  IsVar    -> Nothing

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
  applyM s r@(RowArg v) = case s^.mgu.at (Var v) of
    Nothing -> return r
    Just (AVar v')    -> case cast (RowArg v') of
      Just h  -> do
        tell (Any True)
        return h
      Nothing -> error "PANIC: illegal substitution"
    Just (AnEntity t) -> case cast (RowArg t) of
      Just h -> do
        tell (Any True)
        return h
      Nothing -> error "PANIC: illegal substitution"
  {-# INLINEABLE applyM #-}

  vars f (RowAp l r)  = RowAp <$> vars f l <*> vars f r
  vars f (RowMap k x) = RowMap k <$> vars f x
  vars _ (RowPure a)  = pure (RowPure a)
  vars f (RowArg v)   = f (Var v) <&> \(Var u) ->
    RowArg (fromMaybe (error "PANIC: very illegal substitution" `asTypeOf` v) (cast u))
  {-# INLINEABLE vars #-}

matches :: Row a -> Row b -> Bool
matches a b = has _Just $ evalStateT (match a b) (mempty :: Subst)

match :: (MonadState s m, HasSubst s, MonadPlus m) => Row a -> Row b -> m (Row a)
match (RowAp l r) (RowAp l' r')   = liftM2 RowAp (match l l') (match r r')
match (RowMap f x) (RowMap _ y)   = RowMap f `liftM` match x y
match (RowPure a) RowPure{}       = return $ RowPure a
match l@(RowArg t) r@(RowArg t')  = case term `withArgType` t of
  IsVar -> do
    u <- use subst
    case u^.mgu.at (Var t) of
      Just (AVar t'') -> do
         h <- match (RowArg t'') r
         maybe (error "broken row") return $ cast h
      Just (AnEntity t'') -> do
         h <- match (RowArg t'') r
         maybe (error "broken row") return $ cast h
      Nothing -> do
         unless (eqTerm t t') $ subst %= \x -> (t ~> t') <> x
         maybe (error "broken row") return  $ cast (RowArg t')
  IsEntity -> case term `withArgType` t' of
    IsVar -> do
      u <- use subst
      case u^.mgu.at (Var t') of
        Just (AVar t'')     -> match l (RowArg t'')
        Just (AnEntity t'') -> match l (RowArg t'')
        Nothing -> do
          unless (eqTerm t t') $ subst %= \x -> (t' ~> t) <> x
          return l
    IsEntity
       | cast t == Just t' -> return l
       | otherwise         -> fail "unification"
match _ _ = error "broken row"
{-# INLINEABLE match #-}

withArgType :: t a -> a -> t a
withArgType = const
{-# INLINE withArgType #-}

-}
