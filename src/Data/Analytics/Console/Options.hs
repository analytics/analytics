{-# LANGUAGE DeriveDataTypeable #-}
{-# OPTIONS_GHC -fno-cse #-}
module Data.Analytics.Console.Options
  ( Options(..)
  , mode
  , version
  ) where

import Data.Data
import Data.Version as Version
import System.Console.CmdArgs.Implicit
-- import System.FilePath
import qualified Paths_analytics as Paths

version :: Version
version = Paths.version

data Options
  = Console { datadir :: FilePath, files :: [FilePath] }
  | Run     { datadir :: FilePath, files :: [FilePath] }
  | Daemon  { datadir :: FilePath, files :: [FilePath] }
  deriving (Eq, Show, Data, Typeable)

mode :: IO (Mode (CmdArgs Options))
mode = do
  defaultDataDir <- Paths.getDataDir
  let console = Console
        { files = def &= help "input files" &= typFile -- &= args
        , datadir = def &= opt defaultDataDir &= help "Select the location of the database" &= typDir
        } &= help "Start a console session"

      run = Run
        { files = def &= help "input files" &= typFile -- &= args
        , datadir = def &= opt defaultDataDir &= help "Select the location of the database" &= typDir
        } &= help "Run datalog programs and quit"
          &= auto

      daemon = Daemon
        { files = def &= help "input files" &= typFile -- &= args
        , datadir = def &= opt defaultDataDir &= help "Select the location of the database" &= typDir
        } &= help "Run as a daemon"

  return $ cmdArgsMode $ modes [console, daemon, run]
        &= help "What's yours is mined. What's mined is yours."
        &= summary ("Analytics " ++ showVersion Paths.version ++ ", © Edward Kmett 2013")
        &= verbosity
        &= program "edb"
