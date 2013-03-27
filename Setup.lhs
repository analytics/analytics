#!/usr/bin/runhaskell
\begin{code}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wall #-}
module Main (main) where

import Control.Exception as E
import Control.Monad
import Data.Functor
import Data.List ( nub, foldl1' )
import Data.Version ( showVersion )
import Distribution.Package
import Distribution.PackageDescription as PackageDescription
import Distribution.PackageDescription.Parse
import Distribution.Simple
import Distribution.Simple.Command
import Distribution.Simple.Utils
import Distribution.Simple.BuildPaths
import Distribution.Simple.Setup as Setup
import Distribution.Simple.LocalBuildInfo
import Distribution.System
import Distribution.Text
import Distribution.Verbosity
import System.Directory
import System.Environment
import System.FilePath ( (</>) )
import System.IO.Error
import System.Posix.Directory
import System.Process hiding (env)

unlessResultNewer :: FilePath -> [FilePath] -> IO a -> IO ()
unlessResultNewer resFP sourceFPs act  = do
        resTime <- getModificationTime resFP
        sourceTimes <- mapM getModificationTime sourceFPs
        unless (resTime > foldl1' max sourceTimes ) $ () <$ act
  `E.catch` \ (_ :: SomeException) -> () <$ act

setupAutoTools :: IO ()
setupAutoTools = do
  unlessResultNewer "aclocal.m4"  ["m4/ax_c_have_attribute.m4","m4/ax_c_have_attribute_cold.m4", "m4/ax_check_defined.m4","m4/ax_gcc_x86_cpuid.m4"]$ readProcessWithExitCode "aclocal" ["-Im4"] ""
  unlessResultNewer "config.h.in" ["Makefile.in", "analytics.buildinfo.in", "configure.ac"] $ readProcessWithExitCode "autoreconf" ["-Im4", "-i"] ""

haddockOutputDir :: Package pkg => HaddockFlags -> pkg -> FilePath
haddockOutputDir flags pkg = destDir where
  baseDir = case haddockDistPref flags of
    NoFlag -> "."
    Setup.Flag x -> x
  destDir = baseDir </> "doc" </> "html" </> display (packageName pkg)

main :: IO ()
main = defaultMainWithHooks autoconfUserHooks
  {
    sDistHook = \pkg mlbi hooks flags -> do
     setupAutoTools
     sDistHook autoconfUserHooks pkg mlbi hooks flags
  , buildHook = \pkg lbi hooks flags -> do
     generateBuildModule (fromFlag (buildVerbosity flags)) pkg lbi
     buildHook autoconfUserHooks pkg lbi hooks flags
  , postHaddock = \args flags pkg lbi -> do
     _ <- readProcessWithExitCode "sh" ["scripts/overview.sh"] ""
     copyFiles normal (haddockOutputDir flags pkg) [("images","overview.png")]
     postHaddock autoconfUserHooks args flags pkg lbi
  , postClean = \args flags pkg lbi -> do
     putStrLn "Cleaning up"
     _ <- readProcessWithExitCode "make" ["distclean"] ""
     doesFileExist "analytics.buildinfo" >>= \b -> when b $ removeFile "analytics.buildinfo"
     doesFileExist "config.h.in~" >>= \b -> when b $ removeFile "config.h.in~"
     postClean autoconfUserHooks args flags pkg lbi
  , instHook = \pkg lbi hooks flags -> do
     putStrLn "Registering man page analytics.1"
     _ <- readProcessWithExitCode "make" ["install"] ""
     instHook autoconfUserHooks pkg lbi hooks flags
  , postConf = \args flags pkg lbi -> do
      let verbosity = fromFlag (configVerbosity flags)
      noExtraFlags args
      setupAutoTools
      unlessResultNewer (buildDir lbi </> "autogen" </> "config.h") ["configure", "Setup.lhs"] $ do
        runMyConfigureScript verbosity False flags lbi
      pbi <- getHookedBuildInfo verbosity
      let pkg' = updatePackageDescription pbi pkg
      postConf simpleUserHooks args flags pkg' lbi
  }


getHookedBuildInfo :: Verbosity -> IO HookedBuildInfo
getHookedBuildInfo verbosity = do
  maybe_infoFile <- defaultHookedPackageDesc
  case maybe_infoFile of
    Nothing       -> return emptyHookedBuildInfo
    Just infoFile -> do
      info verbosity $ "Reading parameters from " ++ infoFile
      readHookedBuildInfo verbosity infoFile

runMyConfigureScript :: Verbosity -> Bool -> ConfigFlags -> LocalBuildInfo -> IO ()
runMyConfigureScript verbosity backwardsCompatHack flags lbi = do
  env <- getEnvironment
  let programConfig = withPrograms lbi
  (ccProg, ccFlags) <- configureCCompiler verbosity programConfig
  -- The C compiler's compilation and linker flags (e.g.
  -- "C compiler flags" and "Gcc Linker flags" from GHC) have already
  -- been merged into ccFlags, so we set both CFLAGS and LDFLAGS
  -- to ccFlags
  -- We don't try and tell configure which ld to use, as we don't have
  -- a way to pass its flags too
  here <- getWorkingDirectory
  let env' = appendToEnvironment ("CFLAGS",  unwords ccFlags)
             env
      args' = args here ++ ["--with-gcc=" ++ ccProg]
  -- Run the configure script from the autogen folder
  let autoDir = buildDir lbi </> "autogen"
  createDirectoryIfMissing True autoDir
  changeWorkingDirectory autoDir
  handleNoWindowsSH $
    rawSystemExitWithEnv verbosity "/bin/sh" args' env'
  changeWorkingDirectory here
  copyFile (autoDir </> "analytics.buildinfo") "analytics.buildinfo"
  where
    args there = (there </> "configure") : configureArgs backwardsCompatHack flags

    appendToEnvironment (key, val) [] = [(key, val)]
    appendToEnvironment (key, val) (kv@(k, v) : rest)
     | key == k  = (key, v ++ " " ++ val) : rest
     | otherwise = kv : appendToEnvironment (key, val) rest

    handleNoWindowsSH action
      | buildOS /= Windows = action
      | otherwise = action
          `E.catch` \ioe -> if isDoesNotExistError ioe
                              then die notFoundMsg
                              else throwIO ioe

    notFoundMsg = "The package has a './configure' script. This requires a "
               ++ "Unix compatibility toolchain such as MinGW+MSYS or Cygwin."


generateBuildModule :: Verbosity -> PackageDescription -> LocalBuildInfo -> IO ()
generateBuildModule verbosity pkg lbi = do
  let dir = autogenModulesDir lbi
  createDirectoryIfMissingVerbose verbosity True dir
  withLibLBI pkg lbi $ \_ libcfg -> do
    withTestLBI pkg lbi $ \suite suitecfg -> do
      rewriteFile (dir </> "Build_" ++ testName suite ++ ".hs") $ unlines
        [ "module Build_" ++ testName suite ++ " (deps, buildDir) where"
        , ""
        , "deps :: [String]"
        , "deps = " ++ (show $ formatdeps (testDeps libcfg suitecfg))
        , ""
        , "buildDir :: FilePath"
        , "buildDir = " ++ show (buildDir lbi)
        ]
  where
    formatdeps = map (formatone . snd)
    formatone p = case packageName p of
      PackageName n -> n ++ "-" ++ showVersion (packageVersion p)

testDeps :: ComponentLocalBuildInfo -> ComponentLocalBuildInfo -> [(InstalledPackageId, PackageId)]
testDeps xs ys = nub $ componentPackageDeps xs ++ componentPackageDeps ys

\end{code}
