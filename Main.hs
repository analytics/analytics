module Main (main) where

import Data.Analytics.Console.Unicode

main :: IO ()
main = withUnicode $ do
  putStrLn "Hello World"
