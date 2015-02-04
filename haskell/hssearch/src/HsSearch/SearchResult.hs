module HsSearch.SearchResult
  ( SearchResult(..)
  , blankSearchResult
  , formatSearchResult
  ) where

import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BC
import Data.Char (isSpace)

import HsSearch.SearchSettings

data SearchResult = SearchResult {
                                   searchPattern :: String
                                 , filePath :: FilePath
                                 , lineNum :: Int
                                 , matchStartIndex :: Int
                                 , matchEndIndex :: Int
                                 , line :: B.ByteString
                                 , beforeLines :: [B.ByteString]
                                 , afterLines :: [B.ByteString]
                                 } deriving (Show, Eq)

blankSearchResult :: SearchResult
blankSearchResult = SearchResult {
                                   searchPattern=""
                                 , filePath=""
                                 , lineNum=0
                                 , matchStartIndex=0
                                 , matchEndIndex=0
                                 , line=B.empty
                                 , beforeLines=[]
                                 , afterLines=[]
                                 }

formatSearchResult :: SearchSettings -> SearchResult -> String
formatSearchResult settings result = if (lineNum result) == 0
                                     then formatBinaryResult
                                     else formatTextResult
  where formatBinaryResult = filePath result ++ " matches"
        beforeOrAfter = any (>0) [linesBefore settings, linesAfter settings]
        formatTextResult = if beforeOrAfter
                           then formatMultiLine result
                           else formatSingleLine result

formatSingleLine :: SearchResult -> String
formatSingleLine result =
  filePath result ++ ": " ++
  show (lineNum result) ++ ": [" ++
  show (matchStartIndex result) ++ ":" ++
  show (matchEndIndex result) ++ "]: " ++
  trimLeadingWhitespace (BC.unpack (line result))

formatMultiLine :: SearchResult -> String
formatMultiLine result =
  ((take 80 . repeat) '=') ++ "\n" ++
  filePath result ++ ": " ++
  show (lineNum result) ++ " [" ++
  show (matchStartIndex result) ++ ":" ++
  show (matchEndIndex result) ++ "]:" ++ "\n" ++
  ((take 80 . repeat) '-') ++ "\n" ++
  unlines (map formatLine (zip beforeLineNums resultBeforeLines)) ++
  "> " ++ padNumString (show resultLineNum) ++ " | " ++
  BC.unpack (line result) ++ "\n" ++
  unlines (map formatLine (zip afterLineNums resultAfterLines))
  where resultLineNum = lineNum result
        resultBeforeLines = (map BC.unpack (beforeLines result))
        resultAfterLines = (map BC.unpack (afterLines result))
        maxNumWidth = numWidth (resultLineNum + length resultAfterLines)
        numWidth :: Int -> Int
        numWidth n = recNumWidth 1 10 n
        recNumWidth w m n | n < m = w
                          | otherwise = recNumWidth (w + 1) (m * 10) n
        padNumString ns | length ns < maxNumWidth = padNumString (' ':ns)
                        | otherwise = ns
        firstLineNum = resultLineNum - (length resultBeforeLines)
        beforeLineNums = [firstLineNum..(resultLineNum-1)]
        afterLineNums = [(resultLineNum+1)..(length resultAfterLines + resultLineNum)]
        formatLine (n,l) = "  " ++ padNumString (show n) ++ " | " ++ l

trimLeadingWhitespace :: String -> String
trimLeadingWhitespace s = dropWhile isSpace s
