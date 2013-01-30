--------------------------------------------------------------------
-- |
-- Module    :  Analytics.Variable
-- Copyright :  (c) Edward Kmett 2013
-- License   :  BSD3
-- Maintainer:  Edward Kmett <ekmett@gmail.com>
-- Stability :  experimental
-- Portability: non-portable
--
--------------------------------------------------------------------
module Analytics.Variable where

import Analytics.Match
import Control.Applicative
import Control.Lens
import Data.Data

class (Data (v Int), Match v) => Variable v where
  var :: Prism (v a) (v b) a b

-- | Default definition of 'match' for a 'Var'
matchVar :: (Variable t, Eq (t c)) => (a -> b -> c) -> t a -> t b -> Maybe (t c)
matchVar f va vb = case var Left va of
  Left a -> Just (f a <$> vb)
  Right a' -> case var Left vb of
    Left b -> Just ((`f` b) <$> va)
    Right b' | a' == b'  -> Just a'
             | otherwise -> Nothing
