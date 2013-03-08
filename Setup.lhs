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
import Distribution.Package ( PackageName(PackageName), Package(..), PackageId, InstalledPackageId, packageVersion, packageName )
import Distribution.PackageDescription ( PackageDescription(), TestSuite(..) )
import Distribution.Simple ( defaultMainWithHooks, UserHooks(..), autoconfUserHooks )
import Distribution.Simple.Utils ( rewriteFile, createDirectoryIfMissingVerbose, copyFiles )
import Distribution.Simple.BuildPaths ( autogenModulesDir )
import Distribution.Simple.Setup ( HaddockFlags(..), BuildFlags(buildVerbosity), fromFlag, haddockDistPref, Flag(..) )
import Distribution.Simple.LocalBuildInfo ( withLibLBI, withTestLBI, LocalBuildInfo(), ComponentLocalBuildInfo(componentPackageDeps) )
import Distribution.Text ( display )
import Distribution.Verbosity ( Verbosity, normal )
import System.Directory
import System.FilePath ( (</>) )
import System.Process

--unlessFileExists :: FilePath -> IO a -> IO ()
--unlessFileExists fp act = doesFileExist fp >>= \b -> unless b $ () <$ act

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
    Flag x -> x
  destDir = baseDir </> "doc" </> "html" </> display (packageName pkg)

main :: IO ()
main = defaultMainWithHooks autoconfUserHooks
  { preConf = \args flags -> do
     setupAutoTools
     preConf autoconfUserHooks args flags
  , sDistHook = \pkg mlbi hooks flags -> do
     setupAutoTools
     sDistHook autoconfUserHooks pkg mlbi hooks flags
  , buildHook = \pkg lbi hooks flags -> do
     generateBuildModule (fromFlag (buildVerbosity flags)) pkg lbi
     buildHook autoconfUserHooks pkg lbi hooks flags
  , postHaddock = \args flags pkg lbi -> do
     copyFiles normal (haddockOutputDir flags pkg) [] -- [("doc","analytics.png")]
     postHaddock autoconfUserHooks args flags pkg lbi
  , postClean = \args flags pkg lbi -> do
     putStrLn "Cleaning up"
     _ <- readProcessWithExitCode "make" ["distclean"] ""
     postClean autoconfUserHooks args flags pkg lbi
  , instHook = \pkg lbi hooks flags -> do
     putStrLn "Registering man page analytics.1"
     _ <- readProcessWithExitCode "make" ["install"] ""
     instHook autoconfUserHooks pkg lbi hooks flags
  }

generateBuildModule :: Verbosity -> PackageDescription -> LocalBuildInfo -> IO ()
generateBuildModule verbosity pkg lbi = do
  let dir = autogenModulesDir lbi
  createDirectoryIfMissingVerbose verbosity True dir
  withLibLBI pkg lbi $ \_ libcfg -> do
    withTestLBI pkg lbi $ \suite suitecfg -> do
      rewriteFile (dir </> "Build_" ++ testName suite ++ ".hs") $ unlines
        [ "module Build_" ++ testName suite ++ " where"
        , "deps :: [String]"
        , "deps = " ++ (show $ formatdeps (testDeps libcfg suitecfg))
        ]
  where
    formatdeps = map (formatone . snd)
    formatone p = case packageName p of
      PackageName n -> n ++ "-" ++ showVersion (packageVersion p)

testDeps :: ComponentLocalBuildInfo -> ComponentLocalBuildInfo -> [(InstalledPackageId, PackageId)]
testDeps xs ys = nub $ componentPackageDeps xs ++ componentPackageDeps ys

\end{code}
