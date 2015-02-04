module Main (main) where

import HsSearch.FileUtilTest
import HsSearch.SearchOptionsTest
import HsSearch.SearchSettingsTest

import Test.Framework
import Test.Framework.Providers.HUnit
import Test.HUnit hiding (Test)

main :: IO ()
main = do
  -- FileUtil tests
  fileUtilTests <- getFileUtilTests

  -- SearchOptions tests
  settingsFromArgsTests <- getSettingsFromArgsTests
  settingsFromNoArgsTests <- getSettingsFromNoArgsTests
  archivesOnlyTests <- getArchivesOnlyTests
  debugTests <- getDebugTests

  -- SearchOptions tests
  defaultSearchSettingsTests <- getDefaultSearchSettingsTests
  newExtensionsTests <- getNewExtensionsTests

  defaultMain (fileUtilTests ++ settingsFromArgsTests ++
  	settingsFromNoArgsTests ++ archivesOnlyTests ++ debugTests ++
    defaultSearchSettingsTests ++ newExtensionsTests)
