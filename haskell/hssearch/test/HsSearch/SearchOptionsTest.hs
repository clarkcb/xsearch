module HsSearch.SearchOptionsTest
  ( getArchivesOnlyTests
  , getDebugTests
  , getSettingsFromArgsTests
  , getSettingsFromNoArgsTests
  ) where

import HsSearch.SearchOptions
import HsSearch.SearchSettings

import Test.Framework
import Test.Framework.Providers.HUnit
import Test.HUnit hiding (Test)

getSettingsFromNoArgsTests :: IO [Test]
getSettingsFromNoArgsTests = do
  searchOptions <- getSearchOptions
  case settingsFromArgs searchOptions [] of
    Left _ -> return [ testCase "getSettingsFromNoArgsTests" (True @?= False) ]
    Right settings ->
      return [ testCase "archivesOnly" (archivesOnly settings @?= False)
             , testCase "debug" (debug settings @?= False)
             , testCase "firstMatch" (firstMatch settings @?= False)
             , testCase "includeHidden" (includeHidden settings @?= False)
             , testCase "linesAfter" (linesAfter settings @?= 0)
             , testCase "linesBefore" (linesBefore settings @?= 0)
             , testCase "maxSize" (maxSize settings @?= 0)
             , testCase "minSize" (minSize settings @?= 0)
             , testCase "multiLineSearch" (multiLineSearch settings @?= False)
             , testCase "printDirs" (printDirs settings @?= False)
             , testCase "printFiles" (printFiles settings @?= False)
             , testCase "printLines" (printLines settings @?= False)
             , testCase "printResults" (printResults settings @?= True)
             , testCase "printUsage" (printUsage settings @?= False)
             , testCase "printVersion" (printVersion settings @?= False)
             , testCase "recursive" (recursive settings @?= True)
             , testCase "searchArchives" (searchArchives settings @?= False)
             , testCase "sortCaseInsensitive" (sortCaseInsensitive settings @?= False)
             , testCase "sortDescending" (sortDescending settings @?= False)
             , testCase "uniqueLines" (uniqueLines settings @?= False)
             , testCase "verbose" (verbose settings @?= False)
             ]

getSettingsFromArgsTests :: IO [Test]
getSettingsFromArgsTests = do
  let args = ["-x","hs","-X","hi,o","-s","Searcher","-b","2","-B","2","."]
  searchOptions <- getSearchOptions
  case settingsFromArgs searchOptions args of
    Left _ -> return [ testCase "getSettingsFromArgsTests" (True @?= False) ]
    Right settings ->
      return [ testCase "paths ." (paths settings @?= ["."])
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
  case settingsFromArgs searchOptions args of
    Left _ -> return [ testCase "getArchivesOnlyTests" (True @?= False) ]
    Right settings ->
      return [ testCase "archivesOnly" (archivesOnly settings @?= True)
             , testCase "searchArchives" (searchArchives settings @?= True)
             ]

getDebugTests :: IO [Test]
getDebugTests = do
  let args = ["--debug"]
  searchOptions <- getSearchOptions
  case settingsFromArgs searchOptions args of
    Left _ -> return [ testCase "getDebugTests" (True @?= False) ]
    Right settings ->
      return [ testCase "debug" (debug settings @?= True)
             , testCase "verbose" (verbose settings @?= True)
             ]
