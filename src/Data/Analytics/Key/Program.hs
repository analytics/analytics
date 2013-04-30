{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE PatternGuards #-}
{-# LANGUAGE TemplateHaskell #-}
module Data.Analytics.Key.Program
  ( Program(..)
  , Source
  , compile
  ) where

import Control.Applicative
import Control.Lens
import Data.Approximate.Type
import Data.Analytics.Key.Schedule
import Data.Analytics.Key.Interleaving
import Data.Data
import Data.List
import Data.Void

data Program
  = Take          {-# UNPACK #-} !Int {-# UNPACK #-} !Source Program Program
  | TakeUnfailing {-# UNPACK #-} !Int {-# UNPACK #-} !Source Program
  | Repeat        {-# UNPACK #-} !Int Program Program
  | Loop Program
  | Halt
  deriving (Show,Typeable,Data)

instance Plated Program

type Source = Int

data InterleavedBit = InterleavedBit
  { _bitSource    :: {-# UNPACK #-} !Int
  , __bitUnfailing :: !Bool
  , _bitPriority  :: {-# UNPACK #-} !Int
  } deriving (Show,Typeable,Data)

bit :: Int -> Schedule a -> [InterleavedBit]
bit i s = InterleavedBit i False <$> [s^.schedulePriority,s^.schedulePriority + s^.scheduleStride..]

makeLenses ''InterleavedBit

-- TODO: generate Loop (and Repeat?) constructors properly to deal with infinite keys
compile :: [Schedule Void] -> Program
compile [] = Halt
compile xs = rewrite condense $ go (as ++ bs)
  where
    int = ifoldMap (\i x -> interleavingBy x (bit i x)) xs
    (as,bs) = span (\a -> (a^.bitPriority,a^.bitSource) < normalThreshold) $ unfoldr uncons int
    go :: [InterleavedBit] -> Program
    go []                            = Halt -- should never happen
    go (InterleavedBit s False _:ys) | next <- go ys = Take 1 s (dropSource s next) next
    go (InterleavedBit s True _:ys)  = TakeUnfailing 1 s (go ys)
    normalThreshold  = maximum $ imap (\i x -> (normalAfter x, i)) xs
    normalAfter s
      | sb <- s^.scheduleBits, sb^.confidence == 1 = s^.schedulePriority + (sb^.lo * s^.scheduleStride)
      | otherwise = s^.schedulePriority
    condense (Take m s l (Take n t _ rr)) | s == t = return $ Take (m + n) s l rr
    condense (TakeUnfailing m s (TakeUnfailing n t rr)) | s == t = return $ TakeUnfailing (m + n) s rr
    condense (Loop (Loop m)) = return $ Loop m
    condense (Repeat n (Repeat m b Halt) r) = return $ Repeat (n * m) b r
    condense _ = Nothing

dropSource :: Source -> Program -> Program
dropSource _ Halt = Halt
dropSource s (Take n t l r)
  | s == t    = dropSource s r
  | otherwise = Take n t (dropSource s l) (dropSource s r)
dropSource s (TakeUnfailing n t r) -- this one is odd, how would this happen!?
  | s == t    = dropSource s r
  | otherwise = TakeUnfailing n t (dropSource s r)
dropSource s (Loop p) = case dropSource s p of
  Halt -> Halt
  r -> Loop r
dropSource s (Repeat n p q) = case dropSource s p of
  Halt -> dropSource s q
  r -> Repeat n r (dropSource s q)
