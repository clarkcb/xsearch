module HsSearch.SearchResultTest
  ( getBinaryFileSearchResultTests
  , getMultiLineSearchResultTests
  , getSingleLineSearchResultTests
  ) where

import qualified Data.ByteString.Char8 as BC
import HsSearch.Config
import HsSearch.SearchResult
import HsSearch.SearchSettings

import Test.Framework
import Test.Framework.Providers.HUnit
import Test.HUnit hiding (Test)

testFileLineNum = 10
testFileMatchStartIndex = 15
testFileMatchEndIndex = 23
testFileLine = BC.pack "\tpublic class Searcher\n"

getBinaryFileSearchResultTests :: IO [Test]
getBinaryFileSearchResultTests = do
  xsearchPath <- getXsearchPath
  let binaryFilePath = xsearchPath ++ "/csharp/CsSearch/CsSearch/Searcher.exe"
  let binaryFileSearchResult = blankSearchResult { filePath=binaryFilePath
                                                 , lineNum=0
                                                 , matchStartIndex=0
                                                 , matchEndIndex=0
                                                 }
  let settings = defaultSearchSettings
  let formattedResult = formatSearchResult settings binaryFileSearchResult
  let expectedFormat = binaryFilePath ++ " matches at [0:0]"
  return [testCase "binaryFileSearchResult" (formattedResult @?= expectedFormat)]

getSingleLineSearchResultTests :: IO [Test]
getSingleLineSearchResultTests = do
  xsearchPath <- getXsearchPath
  let testFilePath = xsearchPath ++ "/csharp/CsSearch/CsSearch/Searcher.cs"
  let singleLineSearchResult = blankSearchResult { filePath=testFilePath
                                                 , lineNum=testFileLineNum
                                                 , matchStartIndex=testFileMatchStartIndex
                                                 , matchEndIndex=testFileMatchEndIndex
                                                 , line=testFileLine
                                                 }
  let settings = defaultSearchSettings { colorize=False }
  let formattedResult = formatSearchResult settings singleLineSearchResult
  let expectedFormat = testFilePath ++ ": " ++ show testFileLineNum ++ ": [" ++
                       show testFileMatchStartIndex ++ ":" ++
                       show testFileMatchEndIndex ++ "]: " ++
                       trimLeadingWhitespace (BC.unpack testFileLine)
  return [testCase "singleLineSearchResult" (formattedResult @?= expectedFormat)]

getMultiLineSearchResultTests :: IO [Test]
getMultiLineSearchResultTests = do
  xsearchPath <- getXsearchPath
  let testFilePath = xsearchPath ++ "/csharp/CsSearch/CsSearch/Searcher.cs"
  let lb = [ BC.pack "namespace CsSearch\n"
           , BC.pack "{\n" ]
  let la = [ BC.pack "\t{\n"
           , BC.pack "\t\tprivate readonly FileTypes _fileTypes;\n" ]
  let multiLineSearchResult = blankSearchResult { filePath=testFilePath
                                                , lineNum=testFileLineNum
                                                , matchStartIndex=testFileMatchStartIndex
                                                , matchEndIndex=testFileMatchEndIndex
                                                , line=testFileLine
                                                , beforeLines=lb
                                                , afterLines=la
                                                }
  let settings = defaultSearchSettings { colorize=False, linesBefore=2, linesAfter=2 }
  let formattedResult = formatSearchResult settings multiLineSearchResult
  let expectedFormat = replicate 80 '=' ++ "\n" ++ testFilePath ++ ": " ++
                       show testFileLineNum ++ ": [" ++
                       show testFileMatchStartIndex ++ ":" ++
                       show testFileMatchEndIndex ++ "]\n" ++
                       replicate 80 '-' ++ "\n" ++
                       "   8 | namespace CsSearch\n" ++ 
                       "   9 | {\n" ++ 
                       "> 10 | \tpublic class Searcher\n" ++ 
                       "  11 | \t{\n" ++ 
                       "  12 | \t\tprivate readonly FileTypes _fileTypes;\n"
  return [testCase "multiLineSearchResult" (formattedResult @?= expectedFormat)]
