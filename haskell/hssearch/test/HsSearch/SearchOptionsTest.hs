module HsSearch.SearchOptionsTest
  ( getArchivesOnlyTests
  , getDebugTests
  , getSettingsFromArgsTests
  , getSettingsFromNoArgsTests
  ) where

import HsSearch.Searcher
import HsSearch.SearchOptions
import HsSearch.SearchSettings

import Test.Framework
import Test.Framework.Providers.HUnit
import Test.HUnit hiding (Test)

getSettingsFromNoArgsTests :: IO [Test]
getSettingsFromNoArgsTests = do
  searchOptions <- getSearchOptions
  --let settings = settingsFromArgs searchOptions []
  case settingsFromArgs searchOptions [] of
    Left errMsg -> return []
    Right settings -> do
      return [ testCase "archivesOnly" (archivesOnly settings @?= False)
             , testCase "debug" (debug settings @?= False)
             , testCase "excludeHidden" (excludeHidden settings @?= True)
             , testCase "firstMatch" (firstMatch settings @?= False)
             , testCase "linesAfter" (linesAfter settings @?= 0)
             , testCase "linesBefore" (linesBefore settings @?= 0)
             , testCase "listDirs" (listDirs settings @?= False)
             , testCase "listFiles" (listFiles settings @?= False)
             , testCase "listLines" (listLines settings @?= False)
             , testCase "multiLineSearch" (multiLineSearch settings @?= False)
             , testCase "printResults" (printResults settings @?= True)
             , testCase "printUsage" (printUsage settings @?= False)
             , testCase "printVersion" (printVersion settings @?= False)
             , testCase "recursive" (recursive settings @?= True)
             , testCase "searchArchives" (searchArchives settings @?= False)
             , testCase "uniqueLines" (uniqueLines settings @?= False)
             , testCase "verbose" (verbose settings @?= False)
             ]

getSettingsFromArgsTests :: IO [Test]
getSettingsFromArgsTests = do
  let args = ["-x","hs","-X","hi,o","-s","Searcher","-b","2","-B","2","."]
  searchOptions <- getSearchOptions
  --let settings = settingsFromArgs searchOptions args
  case settingsFromArgs searchOptions args of
    Left errMsg -> return []
    Right settings -> do
      return [ testCase "startpath ." (startPath settings @?= ".")
             , testCase "-s Searcher" (searchPatterns settings @?= ["Searcher"])
             , testCase "-x hs" (inExtensions settings @?= [".hs"])
             , testCase "-X hi,o" (outExtensions settings @?= [".hi", ".o"])
             , testCase "linesAfter" (linesAfter settings @?= 2)
             , testCase "linesBefore" (linesBefore settings @?= 2)
             ]

getArchivesOnlyTests :: IO [Test]
getArchivesOnlyTests = do
  let args = ["--archivesonly"]
  searchOptions <- getSearchOptions
  --let settings = settingsFromArgs searchOptions args
  case settingsFromArgs searchOptions args of
    Left errMsg -> return []
    Right settings -> do
      return [ testCase "archivesOnly" (archivesOnly settings @?= True)
             , testCase "searchArchives" (searchArchives settings @?= True)
             ]

getDebugTests :: IO [Test]
getDebugTests = do
  let args = ["--debug"]
  searchOptions <- getSearchOptions
  --let settings = settingsFromArgs searchOptions args
  case settingsFromArgs searchOptions args of
    Left errMsg -> return []
    Right settings -> do
      return [ testCase "debug" (debug settings @?= True)
             , testCase "verbose" (verbose settings @?= True)
             ]
