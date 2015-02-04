module HsSearch.SearcherTest
  ( getFilterFileTests
  , getIsArchiveSearchFileTests
  , getIsSearchDirTests
  , getIsSearchFileTests
  ) where

import HsSearch.FileTypes
import HsSearch.FileUtil
import HsSearch.Searcher
import HsSearch.SearchFile
import HsSearch.SearchSettings

import Test.Framework
import Test.Framework.Providers.HUnit
import Test.HUnit hiding (Test)

testFile :: FilePath
testFile = "/Users/cary/src/git/xsearch/shared/testFiles/testFile2.txt"

getIsSearchDirTests :: IO [Test]
getIsSearchDirTests = do
  let settings = defaultSearchSettings
  let settingsInDirPattern = settings { inDirPatterns = ["hssearch"] }
  let settingsOutDirPattern = settings { outDirPatterns = ["cssearch"] }
  let settingsIncludeHidden = settings { excludeHidden = False }
  return [ testCase "isSearchDir hssearch default settings" (isSearchDir settings "hssearch" @?= True)
         , testCase "isSearchDir hssearch matching inDirPattern" (isSearchDir settingsInDirPattern "hssearch" @?= True)
         , testCase "isSearchDir hssearch not matching inDirPattern" (isSearchDir settingsInDirPattern "cssearch" @?= False)
         , testCase "isSearchDir hssearch matching outDirPattern" (isSearchDir settingsOutDirPattern "cssearch" @?= False)
         , testCase "isSearchDir hssearch not matching inDirPattern" (isSearchDir settingsOutDirPattern "hssearch" @?= True)
         , testCase "isSearchDir . default settings" (isSearchDir settings "." @?= True)
         , testCase "isSearchDir .. default settings" (isSearchDir settings ".." @?= True)
         , testCase "isSearchDir .git default settings" (isSearchDir settings ".git" @?= False)
         , testCase "isSearchDir .git includeHidden" (isSearchDir settingsIncludeHidden ".git" @?= True)
         ]

getIsSearchFileTests :: IO [Test]
getIsSearchFileTests = do
  let settings = defaultSearchSettings
  let settingsInExtension = settings { inExtensions = [".hs"] }
  let settingsOutExtension = settings { outExtensions = [".cs"] }
  let settingsInFilePattern = settings { inFilePatterns = ["Search"] }
  let settingsOutFilePattern = settings { outFilePatterns = ["Main"] }
  let settingsIncludeHidden = settings { excludeHidden = False }
  return [ testCase "isSearchFile Searcher.hs default settings" (isSearchFile settings "Searcher.hs" @?= True)
         , testCase "isSearchFile Searcher.hs matching inExtensions" (isSearchFile settingsInExtension "Searcher.hs" @?= True)
         , testCase "isSearchFile Searcher.hs not matching inExtensions" (isSearchFile settingsInExtension "Searcher.cs" @?= False)
         , testCase "isSearchFile Searcher.hs matching outExtensions" (isSearchFile settingsOutExtension "Searcher.cs" @?= False)
         , testCase "isSearchFile Searcher.hs not matching outExtensions" (isSearchFile settingsOutExtension "Searcher.hs" @?= True)
         , testCase "isSearchFile Searcher.hs matching inFilePatterns" (isSearchFile settingsInFilePattern "Searcher.hs" @?= True)
         , testCase "isSearchFile Main.hs not matching inFilePatterns" (isSearchFile settingsInFilePattern "Main.hs" @?= False)
         , testCase "isSearchFile Main.hs matching outFilePatterns" (isSearchFile settingsOutFilePattern "Main.hs" @?= False)
         , testCase "isSearchFile Searcher.hs not matching outFilePatterns" (isSearchFile settingsOutFilePattern "Searcher.hs" @?= True)
         , testCase "isSearchFile .gitignore default settings" (isSearchFile settings ".gitignore" @?= False)
         , testCase "isSearchFile .gitignore includeHidden" (isSearchFile settingsIncludeHidden ".gitignore" @?= True)
         ]

getIsArchiveSearchFileTests :: IO [Test]
getIsArchiveSearchFileTests = do
  let settings = defaultSearchSettings
  let settingsInArchiveExtension = settings { inArchiveExtensions = [".zip"] }
  let settingsOutArchiveExtension = settings { outArchiveExtensions = [".gz"] }
  let settingsInArchiveFilePattern = settings { inArchiveFilePatterns = ["arch"] }
  let settingsOutArchiveFilePattern = settings { outArchiveFilePatterns = ["comp"] }
  let settingsIncludeHidden = settings { excludeHidden = False }
  return [ testCase "isArchiveSearchFile archive.zip default settings" (isArchiveSearchFile settings "archive.zip" @?= True)
         , testCase "isArchiveSearchFile archive.zip matching inArchiveExtensions" (isArchiveSearchFile settingsInArchiveExtension "archive.zip" @?= True)
         , testCase "isArchiveSearchFile archive.tar.gz not matching inArchiveExtensions" (isArchiveSearchFile settingsInArchiveExtension "archive.tar.gz" @?= False)
         , testCase "isArchiveSearchFile archive.tar.gz matching outArchiveExtensions" (isArchiveSearchFile settingsOutArchiveExtension "archive.tar.gz" @?= False)
         , testCase "isArchiveSearchFile archive.zip not matching outArchiveExtensions" (isArchiveSearchFile settingsOutArchiveExtension "archive.zip" @?= True)
         , testCase "isArchiveSearchFile archive.zip matching inArchiveFilePatterns" (isArchiveSearchFile settingsInArchiveFilePattern "archive.zip" @?= True)
         , testCase "isArchiveSearchFile compressed.zip not matching inArchiveFilePatterns" (isArchiveSearchFile settingsInArchiveFilePattern "compressed.zip" @?= False)
         , testCase "isArchiveSearchFile compressed.zip matching outArchiveFilePatterns" (isArchiveSearchFile settingsOutArchiveFilePattern "compressed.zip" @?= False)
         , testCase "isArchiveSearchFile archive.zip not matching outArchiveFilePatterns" (isArchiveSearchFile settingsOutArchiveFilePattern "archive.zip" @?= True)
         , testCase "isArchiveSearchFile .gitarchive.zip default settings" (isArchiveSearchFile settings ".gitarchive.zip" @?= False)
         , testCase "isArchiveSearchFile .gitarchive.zip includeHidden" (isArchiveSearchFile settingsIncludeHidden ".gitarchive.zip" @?= True)
         ]

getFilterFileTests :: IO [Test]
getFilterFileTests = do
  let settings = defaultSearchSettings
  let settingsInExtension = settings { inExtensions = [".hs"] }
  let settingsOutExtension = settings { outExtensions = [".hs"] }
  let settingsIncludeHidden = settings { excludeHidden = False }
  let settingsSearchArchives = settings { searchArchives = True }
  let settingsArchivesOnly = settingsSearchArchives { archivesOnly = True }
  let hsTestFile = SearchFile { searchFileContainers=[], searchFilePath="Searcher.hs", searchFileType=Text }
  let hiddenTestFile = SearchFile { searchFileContainers=[], searchFilePath=".gitignore", searchFileType=Text }
  let archiveTestFile = SearchFile { searchFileContainers=[], searchFilePath="archive.zip", searchFileType=Archive }
  return [ testCase "filterFile Searcher.hs default settings" (filterFile settings hsTestFile @?= True)
         , testCase "filterFile Searcher.hs isSearchFile" (filterFile settingsInExtension hsTestFile @?= True)
         , testCase "filterFile Searcher.hs not isSearchFile" (filterFile settingsOutExtension hsTestFile @?= False)
         , testCase "filterFile .gitignore default settings" (filterFile settings hiddenTestFile @?= False)
         , testCase "filterFile .gitignore includeHidden" (filterFile settingsIncludeHidden hiddenTestFile @?= True)
         , testCase "filterFile archive.zip default settings" (filterFile settings archiveTestFile @?= False)
         , testCase "filterFile archive.zip searchArchives" (filterFile settingsSearchArchives archiveTestFile @?= True)
         , testCase "filterFile archive.zip archivesOnly" (filterFile settingsArchivesOnly archiveTestFile @?= True)
         , testCase "filterFile Searcher.hs archivesOnly" (filterFile settingsArchivesOnly hsTestFile @?= False)
         ]

