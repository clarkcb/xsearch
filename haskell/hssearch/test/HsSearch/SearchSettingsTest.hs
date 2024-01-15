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
         , testCase "firstMatch" (firstMatch settings @?= False)
         , testCase "includeHidden" (includeHidden settings @?= False)
         , testCase "linesAfter" (linesAfter settings @?= 0)
         , testCase "linesBefore" (linesBefore settings @?= 0)
         , testCase "listDirs" (listDirs settings @?= False)
         , testCase "listFiles" (listFiles settings @?= False)
         , testCase "listLines" (listLines settings @?= False)
         , testCase "multiLineSearch" (multiLineSearch settings @?= False)
         , testCase "printResults" (printResults settings @?= False)
         , testCase "printUsage" (printUsage settings @?= False)
         , testCase "printVersion" (printVersion settings @?= False)
         , testCase "recursive" (recursive settings @?= True)
         , testCase "searchArchives" (searchArchives settings @?= False)
         , testCase "uniqueLines" (uniqueLines settings @?= False)
         , testCase "verbose" (verbose settings @?= False)
         ]

getNewExtensionsTests :: IO [Test]
getNewExtensionsTests =
  return [ testCase "hs" (newExtensions "hs" @?= [".hs"])
         , testCase "hs,py" (newExtensions "hs,py" @?= [".hs", ".py"])
         ]
