module Main (main) where

import HsSearch.FileUtilTest
import HsSearch.SearcherTest
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

  -- Searcher tests
  isSearchDirTests <- getIsSearchDirTests
  isSearchFileTests <- getIsSearchFileTests
  isArchiveSearchFileTests <- getIsArchiveSearchFileTests

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

  defaultMain (fileUtilTests ++
    isSearchDirTests ++ isSearchFileTests ++ isArchiveSearchFileTests ++
    settingsFromArgsTests ++ settingsFromNoArgsTests ++
    archivesOnlyTests ++ debugTests ++
    binaryFileSearchResultTests ++ singleLineSearchResultTests ++
    multiLineSearchResultTests ++ 
    defaultSearchSettingsTests ++ newExtensionsTests)
