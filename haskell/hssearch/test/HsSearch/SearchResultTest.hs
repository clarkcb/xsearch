module HsSearch.SearchResultTest
  ( getBinaryFileSearchResultTests
  , getMultiLineSearchResultTests
  , getSingleLineSearchResultTests
  ) where

import qualified Data.ByteString.Char8 as BC
import HsSearch.SearchResult
import HsSearch.SearchSettings

import Test.Framework
import Test.Framework.Providers.HUnit
import Test.HUnit hiding (Test)


getBinaryFileSearchResultTests :: IO [Test]
getBinaryFileSearchResultTests = do
  let binaryFilePath = "~/src/git/xsearch/csharp/CsSearch/CsSearch/Searcher.exe"
  let binaryFileSearchResult = blankSearchResult { filePath=binaryFilePath
                                                 , lineNum=0
                                                 , matchStartIndex=0
                                                 , matchEndIndex=0
                                                 }
  let settings = defaultSearchSettings
  let formattedResult = formatSearchResult settings binaryFileSearchResult
  let expectedFormat = binaryFilePath ++ " matches"
  return [testCase "binaryFileSearchResult" (formattedResult @?= expectedFormat)]


getSingleLineSearchResultTests :: IO [Test]
getSingleLineSearchResultTests = do
  let fp = "~/src/git/xsearch/csharp/CsSearch/CsSearch/Searcher.cs"
  let ln = 10
  let msi = 15
  let mei = 23
  let l = BC.pack "\tpublic class Searcher\n"
  let singleLineSearchResult = blankSearchResult { filePath=fp
                                                 , lineNum=ln
                                                 , matchStartIndex=msi
                                                 , matchEndIndex=mei
                                                 , line=l
                                                 }
  let settings = defaultSearchSettings
  let formattedResult = formatSearchResult settings singleLineSearchResult
  let expectedFormat = fp ++ ": " ++ show ln ++ ": [" ++ show msi ++ ":" ++
                       show mei ++ "]: " ++ trimLeadingWhitespace (BC.unpack l)
  return [testCase "singleLineSearchResult" (formattedResult @?= expectedFormat)]


getMultiLineSearchResultTests :: IO [Test]
getMultiLineSearchResultTests = do
  let fp = "~/src/git/xsearch/csharp/CsSearch/CsSearch/Searcher.cs"
  let ln = 10
  let msi = 15
  let mei = 23
  let l = BC.pack "\tpublic class Searcher\n"
  let lb = [ BC.pack "namespace CsSearch\n"
           , BC.pack "{\n" ]
  let la = [ BC.pack "\t{\n"
           , BC.pack "\t\tprivate readonly FileTypes _fileTypes;\n" ]
  let multiLineSearchResult = blankSearchResult { filePath=fp
                                                , lineNum=ln
                                                , matchStartIndex=msi
                                                , matchEndIndex=mei
                                                , line=l
                                                , beforeLines=lb
                                                , afterLines=la
                                                }
  let settings = defaultSearchSettings { linesBefore=2, linesAfter=2 }
  let formattedResult = formatSearchResult settings multiLineSearchResult
  let expectedFormat = ((take 80 . repeat) '=') ++ "\n" ++ fp ++ ": " ++
                       show ln ++ ": [" ++ show msi ++ ":" ++ show mei ++
                       "]:\n" ++ ((take 80 . repeat) '-') ++ "\n" ++
                       "   8 | namespace CsSearch\n" ++ 
                       "   9 | {\n" ++ 
                       "> 10 | \tpublic class Searcher\n" ++ 
                       "  11 | \t{\n" ++ 
                       "  12 | \t\tprivate readonly FileTypes _fileTypes;\n"
  return [testCase "multiLineSearchResult" (formattedResult @?= expectedFormat)]
