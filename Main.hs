module Main (main) where

import Data.Analytics.Console.Unicode
import Data.Analytics.Console.Options

main :: IO ()
main = withUnicode $ do
  opts <- getOptions
  print opts
  -- putStrLn "Hello World"
