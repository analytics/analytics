{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE GADTs #-}
module Analytics.Datalog.Prompt
  ( Step(..)
  , Prompt(..)
  , prompt
  ) where

import qualified Analytics.Datalog.Internal as Datalog
import Analytics.Datalog.Internal hiding (Fact, Query, (:-))
import Data.Typeable

data Step :: (* -> *) -> * -> * where
  Fact  :: (Typeable1 t, Match t) => (forall v. t v) -> Step m ()
  (:-)  :: Ord v => Relation v -> Body v t -> Step m ()
  Query :: Ord v => Body v t -> Step m [t]

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
