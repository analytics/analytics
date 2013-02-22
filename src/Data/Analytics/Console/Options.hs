{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -fno-cse #-}
module Data.Analytics.Console.Options
  ( MonitorOptions(..), _MonitorOptions, HasMonitorOptions(..)
  , Verbosity(..), _Normal, _Verbose
  , CommonOptions(..), HasCommonOptions(..)
  , RunOptions(..), HasRunOptions(..)
  , Command(..), HasCommand(..), _Run
  , Options(..), HasOptions(..)
  , getOptions
  , version
  ) where

import Control.Lens
import Data.Data
import Data.Version as Version
import Options.Applicative
-- import System.FilePath
import qualified Paths_analytics as Paths

version :: Version
version = Paths.version

-- enable/disable EKG
data MonitorOptions = MonitorOptions { _monitorHost :: String, _monitorPort :: Int, _monitorEnabled :: Bool }
  deriving (Eq,Ord,Show,Read,Data,Typeable)

parseMonitorOptions :: Parser MonitorOptions
parseMonitorOptions = MonitorOptions
  <$> strOption (long "ekg-host" <> short 'h' <> help "host for the EKG server" <> metavar "HOST" <> action "hostname" <> value "localhost" <> showDefault)
  <*> option (long "ekg-port" <> short 'p' <> help "port for the EKG server" <> metavar "PORT" <> value 8000 <> showDefault)
  <*> (not <$> switch (long "no-ekg" <> help "do not start the EKG server" <> value False))

makePrisms ''MonitorOptions
makeClassy ''MonitorOptions

data Verbosity = Normal | Verbose
  deriving (Eq,Ord,Show,Read,Data,Typeable)

makePrisms ''Verbosity

data CommonOptions = CommonOptions
  { _commonVerbosity      :: Verbosity
  , _commonDatadir        :: FilePath
  , _commonMonitorOptions :: MonitorOptions
  } deriving (Eq,Ord,Show,Read,Data,Typeable)

parseCommonOptions :: FilePath -> Parser CommonOptions
parseCommonOptions dd = CommonOptions
  <$> flag Normal Verbose (long "verbose" <> short 'v' <> help "Enable verbose mode")
  <*> strOption (long "datadir" <> short 'd' <> metavar "DIR" <> help "Select the location of the database" <> showDefault <> value dd <> action "directory")
  <*> parseMonitorOptions

makeClassy ''CommonOptions

instance HasMonitorOptions CommonOptions where
  monitorOptions = commonMonitorOptions

data RunOptions = RunOptions
  { _runFiles :: [FilePath]
  , _interactive :: Bool
  } deriving (Eq,Ord,Show,Read,Data,Typeable)

makeClassy ''RunOptions

parseRunOptions :: Parser RunOptions
parseRunOptions = RunOptions
  <$> arguments Just (help "files" <> metavar "FILE" <> action "file")
  <*> switch (long "interactive" <> short 'i' <> help "Run a console")

data Command = Run RunOptions
  deriving (Eq,Ord,Show,Read,Data,Typeable)

makePrisms ''Command
makeClassy ''Command

-- TODO: other commands
parseCommand :: Parser Command
parseCommand = Run <$> parseRunOptions

data Options = Options
  { _optionsCommonOptions :: CommonOptions
  , _optionsCommand :: Command
  } deriving (Eq,Ord,Show,Read,Data,Typeable)

makeClassy ''Options

instance HasMonitorOptions Options where
  monitorOptions = commonOptions.monitorOptions

instance HasCommonOptions Options where
  commonOptions = optionsCommonOptions

instance HasCommand Options where
  command = optionsCommand

parseOptions :: FilePath -> Parser Options
parseOptions dd = Options <$> parseCommonOptions dd <*> parseCommand

getOptions :: IO Options
getOptions = do
  ddd <- Paths.getDataDir
  execParser $ info (helper <*> parseOptions ddd) $
    fullDesc
    <> progDesc "Run datalog programs"
    <> header ("Analytics " ++ showVersion Paths.version ++ ", © Edward Kmett 2013")
