{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE FlexibleInstances #-}
module Analytics.Datalog
  ( Relative(..)
  , Body(..), no
  , Datalog(..), query
  ) where

import Analytics.Match
import Control.Applicative
import Control.Monad
import Control.Monad.Trans.Class
import Data.Typeable
import Data.Void

infixr 2 :&
infixr 1 :-

class (Typeable1 t, Match t) => Relative t a r | r -> a where
  rel :: t a -> r

data Rel a = forall t. (Typeable1 t, Match t) => Rel (t a)

instance (Typeable1 t, Match t) => Relative t a (Rel a) where
  rel ta = Rel ta


data Body r a where
  Body   :: (Typeable1 t, Match t) => t a -> Body (t Void) a
  Bodies :: [Body t a] -> Body [t] a
  (:&)   :: Body s a -> Body t a -> Body (s, t) a
  No     :: Rel a -> Body () a

instance (Typeable1 s, Match s, t ~ s Void) => Relative s a (Body t a) where
  rel ta = Body ta

no :: Rel a -> Body () a
no = No

instance (Typeable1 t, Match t, u ~ ()) => Relative t Void (Datalog m u) where
  rel tv = Fact (vacuous tv)

query :: Ord a => Body t a -> Datalog m [t]
query = Query

-- This is designed to maximize the ease of construction, not use!
data Datalog :: (* -> *) -> * -> * where
  Fact  :: (Typeable1 t, Match t) => (forall a. t a) -> Datalog m ()
  (:-)  :: Ord a => Rel a -> Body t a -> Datalog m ()
  Query :: Ord a => Body t a -> Datalog m [t]
  Bind  :: Datalog m a -> (a -> Datalog m b) -> Datalog m b
  Pure  :: a -> Datalog m a
  Lift  :: m a -> Datalog m a

instance Functor (Datalog m) where
  fmap = liftM

instance Applicative (Datalog m) where
  pure = Pure
  (<*>) = ap

instance Monad (Datalog m) where
  return = Pure
  (>>=) = Bind

instance MonadTrans Datalog where
  lift = Lift
