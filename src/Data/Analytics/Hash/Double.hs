{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
module Data.Analytics.Hash.Double
  ( Hash(..)
  , hashed
  ) where

import Control.Applicative
import Control.Lens
import Data.Data
import Data.Hashable
import Generics.Deriving

-- | \"Less Hashing, Same Performance: Building a Better Bloom Filter\" by
-- Kirsch and Mitzenmacher demonstrated that for many use-cases, especially
-- involving Bloom filters, we can use pairwise independent hashes to
-- generate a family of related hash functions with good characteristics.
--
-- <http://www.eecs.harvard.edu/~kirsch/pubs/bbbf/rsa.pdf>
--
-- This stores a pair of hashes.
--
-- >>> hashed (42 :: Int)^..taking 4 each
-- [-2574874314062730062,-9186383815474761572,2648850756822758536,-3962658744589272970]
--
-- >>> hashed (42 :: Int)^.ix 3
-- -3962658744589272970
data Hash = Hash {-# UNPACK #-} !Int {-# UNPACK #-} !Int
  deriving (Eq,Ord,Show,Read,Data,Typeable,Generic)

hashed :: Hashable a => a -> Hash
hashed a = Hash (hashWithSalt 0xdc36d1615b7400a4 a) -- taken from Data.Hash
                (hashWithSalt 0x53dffa872f4d7341 a) -- chosen by fair die roll
{-# INLINE hashed #-}

type instance Index Hash   = Int
type instance IxValue Hash = Int

instance (Contravariant f, Functor f) => Contains f Hash where
  contains i f _ = coerce $ indexed f (i :: Int) True
  {-# INLINE contains #-}

instance (Contravariant f, Functor f) => Ixed f Hash where
  ix i f (Hash a b) = coerce $ indexed f i (a + i * (b + i))
  {-# INLINE ix #-}

instance (Contravariant f, Applicative f) => Each f Hash Hash Int Int where
  each f (Hash a b) = go 0 where
    go !i = indexed f i (a + i*(b+i)) *> go (i + 1)
  {-# INLINE each #-}
