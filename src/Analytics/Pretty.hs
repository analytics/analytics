{-# OPTIONS_GHC -fno-warn-type-defaults #-}
--------------------------------------------------------------------
-- |
-- Module    :  Analytics.Pretty
-- Copyright :  (c) Edward Kmett and Dan Doel 2012-2013
-- License   :  BSD3
-- Maintainer:  Edward Kmett <ekmett@gmail.com>
-- Stability :  experimental
-- Portability: non-portable
--
--------------------------------------------------------------------
module Analytics.Pretty
  ( module Text.PrettyPrint.ANSI.Leijen
  , Semigroup(..)
  , names
  , parensIf
  , say
  , sayLn
  , utf8
  ) where

import Control.Applicative
import Control.Lens
import Control.Monad.IO.Class
import Data.Semigroup
import Numeric.Lens
import System.IO hiding (utf8)
import Text.PrettyPrint.ANSI.Leijen hiding ((<$>), (<>))
import Text.Trifecta.Delta ()
import Data.ByteString.UTF8 as UTF8

-- | This is an infinitely large free variable supply you can trim your used variables out of.
names :: [String]
names = map pure az
    ++ [ i : review (base 36) j | j <- [1..], i <- az ] where
  az = ['a'..'z']

-- | Pretty print parentheses
parensIf :: Bool -> Doc -> Doc
parensIf True  = parens
parensIf False = id

say :: MonadIO m => Doc -> m ()
say = liftIO . displayIO stdout . renderPretty 0.8 80

sayLn :: MonadIO m => Doc -> m ()
sayLn d = say (d <> linebreak)

-- ignoring replacements
utf8 :: Iso' ByteString String
utf8 = iso UTF8.toString UTF8.fromString
{-# INLINE utf8 #-}
