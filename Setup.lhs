#!/usr/bin/runhaskell
\begin{code}
{-# OPTIONS_GHC -Wall #-}
module Main (main) where

import Control.Monad
import Data.Functor
import Data.List ( nub )
import Data.Version ( showVersion )
import Distribution.Package ( PackageName(PackageName), PackageId, InstalledPackageId, packageVersion, packageName )
import Distribution.PackageDescription ( PackageDescription(extraSrcFiles), TestSuite(..) )
import Distribution.Simple ( defaultMainWithHooks, UserHooks(..), autoconfUserHooks )
import Distribution.Simple.Utils ( rewriteFile, createDirectoryIfMissingVerbose )
import Distribution.Simple.BuildPaths ( autogenModulesDir )
import Distribution.Simple.Setup ( BuildFlags(buildVerbosity), fromFlag )
import Distribution.Simple.LocalBuildInfo ( withLibLBI, withTestLBI, LocalBuildInfo(), ComponentLocalBuildInfo(componentPackageDeps) )
import Distribution.Verbosity ( Verbosity )
import System.Directory
import System.FilePath ( (</>) )
import System.Process

unlessFileExists :: FilePath -> IO a -> IO ()
unlessFileExists fp act = doesFileExist fp >>= \b -> unless b $ () <$ act

setupAutoTools :: IO ()
setupAutoTools = do
  unlessFileExists "aclocal.m4" $ readProcessWithExitCode "aclocal" [] ""
  unlessFileExists "config.h.in" $ readProcessWithExitCode "autoreconf" ["-i"] ""

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
