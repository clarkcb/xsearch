module HsSearch.SearcherTest
  ( getSearchContentsTests
  , getSearchLinesTests
  ) where

import HsFind.FileUtil (getFileByteString, getFileLines)

import HsSearch.Config
import HsSearch.Searcher
import HsSearch.SearchResult
import HsSearch.SearchSettings

import qualified Data.ByteString as B
import Test.Framework
import Test.Framework.Providers.HUnit
import Test.HUnit hiding (Test)

sharedTestFile :: FilePath
sharedTestFile = "/shared/testFiles/testFile2.txt"

getFileContents :: FilePath -> IO B.ByteString
getFileContents f = do
  contentsEither <- getFileByteString f
  case contentsEither of
    (Left _) -> return B.empty
    (Right contents) -> return contents

getSearchContentsTests :: IO [Test]
getSearchContentsTests = do
  let settings = defaultSearchSettings { searchPatterns = ["Searcher"] }
  xsearchPath <- getXsearchPath
  let testFile = xsearchPath ++ sharedTestFile
  contents <- getFileContents testFile
  let results = searchContents settings contents
  return [ testCase "length results == 2" (length results @?= 2)
         , testCase "lineNum (head results) == 29" (lineNum (head results) @?= 29)
         , testCase "matchStartIndex (head results) == 3" (matchStartIndex (head results) @?= 3)
         , testCase "matchEndIndex (head results) == 11" (matchEndIndex (head results) @?= 11)
         , testCase "lineNum (last results) == 35" (lineNum (last results) @?= 35)
         , testCase "matchStartIndex (last results) == 3" (matchStartIndex (last results) @?= 24)
         , testCase "matchEndIndex (last results) == 11" (matchEndIndex (last results) @?= 32)
         ]

getLines :: FilePath -> IO [B.ByteString]
getLines f = do
  linesEither <- getFileLines f
  case linesEither of
    Left e -> return []
    Right fileLines -> return fileLines

getSearchLinesTests :: IO [Test]
getSearchLinesTests = do
  let settings = defaultSearchSettings { searchPatterns = ["Searcher"] }
  xsearchPath <- getXsearchPath
  let testFile = xsearchPath ++ sharedTestFile
  fileLines <- getLines testFile
  let results = searchLines settings fileLines
  return [ testCase "length results == 2" (length results @?= 2)
         , testCase "lineNum (head results) == 29" (lineNum (head results) @?= 29)
         , testCase "matchStartIndex (head results) == 3" (matchStartIndex (head results) @?= 3)
         , testCase "matchEndIndex (head results) == 11" (matchEndIndex (head results) @?= 11)
         , testCase "lineNum (last results) == 35" (lineNum (last results) @?= 35)
         , testCase "matchStartIndex (last results) == 3" (matchStartIndex (last results) @?= 24)
         , testCase "matchEndIndex (last results) == 11" (matchEndIndex (last results) @?= 32)
         ]
