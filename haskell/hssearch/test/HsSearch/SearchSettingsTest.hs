module HsSearch.SearchSettingsTest
  ( getDefaultSearchSettingsTests
  , getNewExtensionsTests
  ) where

import HsSearch.SearchSettings

import Test.Framework
import Test.Framework.Providers.HUnit
import Test.HUnit hiding (Test)

getDefaultSearchSettingsTests :: IO [Test]
getDefaultSearchSettingsTests = do
  let settings = defaultSearchSettings
  return [ testCase "archivesOnly" (archivesOnly settings @?= False)
         , testCase "debug" (debug settings @?= False)
         , testCase "doTiming" (doTiming settings @?= False)
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

getNewExtensionsTests :: IO [Test]
getNewExtensionsTests = do
  return [ testCase "hs" (newExtensions "hs" @?= [".hs"])
         , testCase "hs,py" (newExtensions "hs,py" @?= [".hs", ".py"])
         ]
