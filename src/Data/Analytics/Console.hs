{-# OPTIONS_GHC -fno-warn-unused-do-bind #-}
--------------------------------------------------------------------
-- |
-- Copyright :  (c) Edward Kmett 2013
-- License   :  BSD3
-- Maintainer:  Edward Kmett <ekmett@gmail.com>
-- Stability :  experimental
-- Portability: non-portable
--
--------------------------------------------------------------------
module Data.Analytics.Console (main) where

import Control.Applicative
import Control.Lens
import Data.Analytics.Console.Unicode
import Data.Analytics.Console.Options
import Data.ByteString.Char8 as Char8
import System.Remote.Monitoring
import Prelude as P

main :: IO ()
main = withUnicode $ do
  opts <- getOptions
  print opts
  _ekg <- if opts^.monitorEnabled
          then Just <$> forkServer (Char8.pack $ opts^.monitorHost) (opts^.monitorPort)
          else pure Nothing
  P.putStrLn "Monitor Enabled. Hit Enter to Shutdown."
  P.getLine
  P.putStrLn "Goodbye"
