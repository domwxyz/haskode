{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Small shared test-runner utilities for the no-framework test suite.
module Haskode.Test.Util
  ( Test
  , cleanup
  , createTestTree
  , runTests
  , skipIfNoSymlinks
  , skipOnWindows
  , toolDescriptionFromRegistry
  ) where

import Control.Exception (IOException, try)
import System.Directory  (createDirectory, createFileLink, getTemporaryDirectory,
                          removeDirectoryRecursive, removeFile)
import System.Exit       (exitFailure, exitSuccess)
import System.FilePath   ((</>))
import System.Info       (os)
import qualified Data.Text as T

import Haskode.Tools (Tool (..), defaultRegistry, lookupTool)

-- | Tests return Right for pass, Left with a useful failure message otherwise.
type Test = IO (Either String ())

-- | Helper: remove a temp file, ignoring errors.
cleanup :: FilePath -> IO ()
cleanup path = do
  _ <- try (removeFile path) :: IO (Either IOException ())
  pure ()

-- | Helper: create a temp directory tree for testing.
--   Returns (rootDir, cleanup action).
createTestTree :: IO (FilePath, IO ())
createTestTree = do
  tmpDir <- getTemporaryDirectory
  let root = tmpDir </> "haskode-glob-test"
  _ <- try (removeDirectoryRecursive root) :: IO (Either IOException ())
  createDirectory root
  createDirectory (root </> "src")
  createDirectory (root </> "src" </> "deep")
  createDirectory (root </> "test")
  createDirectory (root </> ".git")
  writeFile (root </> "Main.hs")         "module Main where\nmain = putStrLn \"hi\""
  writeFile (root </> "README.md")       "# Test project"
  writeFile (root </> "Makefile")        "all: build"
  writeFile (root </> "src" </> "Lib.hs")    "module Lib where\nlibFunc = id"
  writeFile (root </> "src" </> "Util.hs")   "module Util where\nutilFunc = id"
  writeFile (root </> "src" </> "deep" </> "Inner.hs") "module Inner where"
  writeFile (root </> "src" </> "data.txt")  "some data"
  writeFile (root </> "test" </> "Spec.hs")  "module Spec where\ntestFunc = id"
  writeFile (root </> ".git" </> "config")   "[core]"
  let cleanupAction = do
        _ <- try (removeDirectoryRecursive root) :: IO (Either IOException ())
        pure ()
  pure (root, cleanupAction)

toolDescriptionFromRegistry :: T.Text -> Maybe T.Text
toolDescriptionFromRegistry name = toolDescription <$> lookupTool name defaultRegistry

-- | Check if the current process can create symbolic links.
--   On Windows this typically requires Administrator privileges.
canCreateSymlinks :: IO Bool
canCreateSymlinks = do
  tmpDir <- getTemporaryDirectory
  let probePath = tmpDir </> "haskode-symlink-probe"
  _ <- try (removeFile probePath) :: IO (Either IOException ())
  result <- try (createFileLink probePath (probePath ++ ".link")) :: IO (Either IOException ())
  case result of
    Left _  -> pure False
    Right _ -> do
      _ <- try (removeFile (probePath ++ ".link")) :: IO (Either IOException ())
      pure True

-- | Skip a test if symlinks are not supported on this platform.
skipIfNoSymlinks :: Test -> Test
skipIfNoSymlinks test = do
  ok <- canCreateSymlinks
  if ok then test else pure $ Right ()

-- | Skip a test on Windows (mingw32).
skipOnWindows :: Test -> Test
skipOnWindows test
  | os == "mingw32" = pure $ Right ()
  | otherwise       = test

runTests :: [Test] -> IO ()
runTests tests = do
  results <- sequence tests
  let failures = [ e | Left e <- results ]
  if null failures
    then putStrLn ("All " ++ show (length results) ++ " tests passed.")
         >> exitSuccess
    else do
      mapM_ (\e -> putStrLn $ "FAIL: " ++ e) failures
      exitFailure
