module Main (main) where

import Data.Analytics.Console.Unicode
import Data.Analytics.Console.Options
import System.Console.CmdArgs hiding (args)
import System.Environment (getArgs, withArgs)

main :: IO ()
main = withUnicode $ do
  m <- mode
  args <- getArgs
  opts <- (if null args then withArgs ["--help"] else id) $ cmdArgsRun m
  print opts
  putStrLn "Hello World"
