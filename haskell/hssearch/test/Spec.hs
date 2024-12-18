module Main (main) where

-- import HsSearch.FileTypesTest
-- import HsSearch.FileUtilTest
import HsSearch.SearcherTest
import HsSearch.SearchOptionsTest
import HsSearch.SearchResultTest
import HsSearch.SearchSettingsTest

import Test.Framework

main :: IO ()
main = do
  -- Searcher tests
  searchLinesTests <- getSearchLinesTests
  searchContentsTests <- getSearchContentsTests

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

  defaultMain (searchLinesTests ++ searchContentsTests ++
    settingsFromArgsTests ++ settingsFromNoArgsTests ++
    archivesOnlyTests ++ debugTests ++
    binaryFileSearchResultTests ++ singleLineSearchResultTests ++
    multiLineSearchResultTests ++ 
    defaultSearchSettingsTests ++ newExtensionsTests)
