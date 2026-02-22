module HsSearch.SearchResultTest
  ( getBinaryFileSearchResultTests
  , getMultiLineSearchResultTests
  , getSingleLineSearchResultTests
  , getSingleLineLongerThanMaxLineLengthSearchResultTests
  , getSingleLineMatchLongerThanMaxLineLengthSearchResultTests
  , getSingleLineMatchLongerThanMaxLineLengthSearchResult2Tests
  ) where

import qualified Data.ByteString.Char8 as BC
import HsSearch.Config
import HsSearch.SearchResult
import HsSearch.SearchSettings

import HsFind.FileResult
import HsFind.FileTypes

import Test.Framework
import Test.Framework.Providers.HUnit
import Test.HUnit hiding (Test)

testFileLineNum :: Int
testFileLineNum = 10

testFileMatchStartIndex :: Int
testFileMatchStartIndex = 15

testFileMatchEndIndex :: Int
testFileMatchEndIndex = 21

testFileLine :: BC.ByteString
testFileLine = BC.pack "\tpublic class Searcher\n"

getBinaryFileSearchResultTests :: IO [Test]
getBinaryFileSearchResultTests = do
  xsearchPath <- getXsearchPath
  let binaryFilePath = xsearchPath ++ "/csharp/CsSearch/CsSearch/Searcher.exe"
  let binaryFileResult = newFileResult binaryFilePath Binary
  let binarySearchResult = blankSearchResult { fileResult=binaryFileResult
                                             , lineNum=0
                                             , matchStartIndex=0
                                             , matchEndIndex=0
                                             }
  let settings = defaultSearchSettings
  let expectedFormat = binaryFilePath ++ " matches at [0:0]"
  let formattedResult = formatSearchResult settings binarySearchResult
  return [testCase "binarySearchResult" (formattedResult @?= expectedFormat)]

getSingleLineSearchResultTests :: IO [Test]
getSingleLineSearchResultTests = do
  xsearchPath <- getXsearchPath
  let testFilePath = xsearchPath ++ "/csharp/CsSearch/CsSearch/Searcher.cs"
  let testFileResult = newFileResult testFilePath Code
  let singleLineSearchResult = blankSearchResult { fileResult=testFileResult
                                                 , lineNum=testFileLineNum
                                                 , matchStartIndex=testFileMatchStartIndex
                                                 , matchEndIndex=testFileMatchEndIndex
                                                 , line=testFileLine
                                                 }
  let settings = defaultSearchSettings { colorize=False }
  let expectedFormat = testFilePath ++ ": " ++ show testFileLineNum ++ ": [" ++
                       show testFileMatchStartIndex ++ ":" ++
                       show testFileMatchEndIndex ++ "]: " ++
                       trimLeadingWhitespace (BC.unpack testFileLine)
  let formattedResult = formatSearchResult settings singleLineSearchResult
  return [testCase "singleLineSearchResult" (formattedResult @?= expectedFormat)]

getSingleLineLongerThanMaxLineLengthSearchResultTests :: IO [Test]
getSingleLineLongerThanMaxLineLengthSearchResultTests = do
  let testFilePath = "./maxlen.txt"
  let testFileResult = newFileResult testFilePath Text
  let matchStartIndex = 53
  let matchEndIndex = 59
  let line = BC.pack "0123456789012345678901234567890123456789012345678901maxlen8901234567890123456789012345678901234567890123456789"
  let singleLineSearchResult = blankSearchResult { fileResult=testFileResult
                                                 , lineNum=testFileLineNum
                                                 , matchStartIndex=matchStartIndex
                                                 , matchEndIndex=matchEndIndex
                                                 , line=line
                                                 }
  let settings = defaultSearchSettings { colorize=False, maxLineLength=100 }
  let expectedLine = "...89012345678901234567890123456789012345678901maxlen89012345678901234567890123456789012345678901..."
  let expectedFormat = testFilePath ++ ": " ++ show testFileLineNum ++ ": [" ++
                       show matchStartIndex ++ ":" ++
                       show matchEndIndex ++ "]: " ++
                       expectedLine
  let formattedResult = formatSearchResult settings singleLineSearchResult
  return [testCase "singleLineLongerThanMaxLineLengthSearchResult" (formattedResult @?= expectedFormat)]

getSingleLineMatchLongerThanMaxLineLengthSearchResultTests :: IO [Test]
getSingleLineMatchLongerThanMaxLineLengthSearchResultTests = do
  let testFilePath = "./maxlen.txt"
  let testFileResult = newFileResult testFilePath Text
  let fileLineNum = 1
  let matchStartIndex = 1
  let matchEndIndex = 110
  let line = BC.pack "0123456789012345678901234567890123456789012345678901maxlen8901234567890123456789012345678901234567890123456789"
  let singleLineSearchResult = blankSearchResult { fileResult=testFileResult
                                                 , lineNum=fileLineNum
                                                 , matchStartIndex=matchStartIndex
                                                 , matchEndIndex=matchEndIndex
                                                 , line=line
                                                 }
  let settings = defaultSearchSettings { colorize=False, maxLineLength=100 }
  let expectedLine = "0123456789012345678901234567890123456789012345678901maxlen890123456789012345678901234567890123456..."
  let expectedFormat = testFilePath ++ ": " ++ show fileLineNum ++ ": [" ++
                       show matchStartIndex ++ ":" ++
                       show matchEndIndex ++ "]: " ++
                       expectedLine
  let formattedResult = formatSearchResult settings singleLineSearchResult
  return [testCase "getSingleLineMatchLongerThanMaxLineLengthSearchResultTests" (formattedResult @?= expectedFormat)]

getSingleLineMatchLongerThanMaxLineLengthSearchResult2Tests :: IO [Test]
getSingleLineMatchLongerThanMaxLineLengthSearchResult2Tests = do
  let testFilePath = "./maxlen.txt"
  let testFileResult = newFileResult testFilePath Text
  let fileLineNum = 1
  let matchStartIndex = 11
  let matchEndIndex = 120
  let line = BC.pack "ABCDEFGHIJ0123456789012345678901234567890123456789012345678901maxlen8901234567890123456789012345678901234567890123456789ABCDEFGHIJ"
  let singleLineSearchResult = blankSearchResult { fileResult=testFileResult
                                                 , lineNum=fileLineNum
                                                 , matchStartIndex=matchStartIndex
                                                 , matchEndIndex=matchEndIndex
                                                 , line=line
                                                 }
  let settings = defaultSearchSettings { colorize=False, maxLineLength=100 }
  let expectedLine = "...3456789012345678901234567890123456789012345678901maxlen890123456789012345678901234567890123456..."
  let expectedFormat = testFilePath ++ ": " ++ show fileLineNum ++ ": [" ++
                       show matchStartIndex ++ ":" ++
                       show matchEndIndex ++ "]: " ++
                       expectedLine
  let formattedResult = formatSearchResult settings singleLineSearchResult
  return [testCase "getSingleLineMatchLongerThanMaxLineLengthSearchResultTests" (formattedResult @?= expectedFormat)]

getMultiLineSearchResultTests :: IO [Test]
getMultiLineSearchResultTests = do
  xsearchPath <- getXsearchPath
  let testFilePath = xsearchPath ++ "/csharp/CsSearch/CsSearch/Searcher.cs"
  let testFileResult = newFileResult testFilePath Code
  let lb = [ BC.pack "namespace CsSearch\n"
           , BC.pack "{\n" ]
  let la = [ BC.pack "\t{\n"
           , BC.pack "\t\tprivate readonly FileTypes _fileTypes;\n" ]
  let multiLineSearchResult = blankSearchResult { fileResult=testFileResult
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
