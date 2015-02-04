module Main (main) where

import HsSearch.FileUtilTest
import HsSearch.SearchOptionsTest
import HsSearch.SearchResultTest
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

  -- SearchResult tests
  binaryFileSearchResultTests <- getBinaryFileSearchResultTests
  singleLineSearchResultTests <- getSingleLineSearchResultTests
  multiLineSearchResultTests <- getMultiLineSearchResultTests

  -- SearchSettings tests
  defaultSearchSettingsTests <- getDefaultSearchSettingsTests
  newExtensionsTests <- getNewExtensionsTests

  defaultMain (fileUtilTests ++ settingsFromArgsTests ++
  	settingsFromNoArgsTests ++ archivesOnlyTests ++ debugTests ++
    binaryFileSearchResultTests ++ singleLineSearchResultTests ++
    multiLineSearchResultTests ++ 
    defaultSearchSettingsTests ++ newExtensionsTests)
