{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE GADTs #-}
--------------------------------------------------------------------
-- |
-- Module    :  Data.Analytics.Prompt
-- Copyright :  (c) Edward Kmett 2013
-- License   :  BSD3
-- Maintainer:  Edward Kmett <ekmett@gmail.com>
-- Stability :  experimental
-- Portability: non-portable
--
--------------------------------------------------------------------
module Data.Analytics.Prompt
  ( Step(..)
  , Prompt(..)
  , prompt
  ) where

import qualified Data.Analytics.Internal.Datalog as Datalog
import Data.Analytics.Internal.Datalog hiding (Fact, Query, (:-))
import Data.Analytics.Internal.Query
import Data.Analytics.Match
import Data.Analytics.Relation
import Data.Typeable

data Step :: (* -> *) -> * -> * where
  Fact  :: (Typeable1 t, Match t) => (forall v. t v) -> Step m ()
  (:-)  :: Ord v => Relation v -> Query v t          -> Step m ()
  Query :: Ord v => Query v t                        -> Step m [t]

data Prompt :: (* -> *) -> * -> * where
  Done :: a -> Prompt m a
  (:>>=) :: Step m a -> (a -> Datalog m b) -> Prompt m b

prompt :: Monad m => Datalog m a -> m (Prompt m a)
prompt (Datalog.Fact xs)          = return $ Fact xs :>>= return
prompt (Datalog.Query q)          = return $ Query q :>>= return
prompt (h Datalog.:- b)           = return $ (h :- b) :>>= return
prompt (Return a)                 = return $ Done a
prompt (Lift m)                   = m >>= return . Done
prompt (Bind (Datalog.Fact xs) g) = return $ Fact xs :>>= g
prompt (Bind (Datalog.Query q) g) = return $ Query q :>>= g
prompt (Bind (h Datalog.:- b) g)  = return $ (h :- b) :>>= g
prompt (Bind (Lift m) g)          = m >>= prompt . g
prompt (Bind (Return a) g)        = prompt (g a)
prompt (Bind m g `Bind` h)        = prompt $ m `Bind` \x -> g x `Bind` h
