module HsSearch.SearchResult
  ( SearchResult(..)
  , blankSearchResult
  , formatBSLine
  , formatLine
  , formatSearchResult
  , searchResultPath
  , sortSearchResults
  , trimLeadingWhitespace
  ) where

import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BC
import Data.Char (isSpace)
import Data.List (sortBy)
import Text.Regex.PCRE

import HsFind.Color (green, reset)
import HsFind.FileResult (FileResult(..), blankFileResult, colorizeString, formatFilePath, getCompareFileResultsFunc)
import HsSearch.SearchSettings

data SearchResult = SearchResult {
                                   searchPattern :: String
                                 , fileResult :: FileResult
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
                                 , fileResult=blankFileResult
                                 , lineNum=0
                                 , matchStartIndex=0
                                 , matchEndIndex=0
                                 , line=B.empty
                                 , beforeLines=[]
                                 , afterLines=[]
                                 }

formatSearchResult :: SearchSettings -> SearchResult -> String
formatSearchResult settings result = if lineNum result == 0
                                     then formatBinaryResult
                                     else formatTextResult
  where formatBinaryResult = fileResultPath (fileResult result) ++ " matches at [" ++
                             show (matchStartIndex result) ++ ":" ++
                             show (matchEndIndex result) ++ "]"
        isMultiline = any (>0) [linesBefore settings, linesAfter settings]
        formatTextResult = if isMultiline
                           then formatMultiLine settings result
                           else formatSingleLine settings result

searchResultPath :: SearchResult -> FilePath
searchResultPath result = fileResultPath (fileResult result)

subByteString :: Int -> Int -> B.ByteString -> B.ByteString
subByteString si ei bs = B.take (ei - si) (B.drop si bs)

colorizeBS :: Int -> Int -> B.ByteString -> B.ByteString
colorizeBS si ei bs =
  BC.concat [subByteString 0 si bs,
             BC.pack green,
             subByteString si ei bs,
             BC.pack reset,
             subByteString ei (BC.length bs) bs]

formatMatchingLine :: SearchSettings -> SearchResult -> String
formatMatchingLine settings result =
  doFormatLine (line result)
  where l = line result
        leadingSpaceCount = BC.length (BC.takeWhile isSpace l)
        trimmedLength = BC.length l - leadingSpaceCount
        maxLineEndIndex = trimmedLength
        matchLength = matchEndIndex result - matchStartIndex result
        msi = matchStartIndex result - 1 - leadingSpaceCount
        mei = msi + matchLength
        decGreaterThanZero :: Int -> Int
        decGreaterThanZero n | n > 0 = n - 1
                             | otherwise = n
        incLessThanMax :: Int -> Int -> Int
        incLessThanMax n maxN | n < maxN = n + 1
                              | otherwise = n
        recGetIndicesToMaxLen :: Int -> Int -> Int -> (Int, Int)
        recGetIndicesToMaxLen s e maxLen | e - s + 1 < maxLen = recGetIndicesToMaxLen (decGreaterThanZero s) (incLessThanMax e maxLineEndIndex) maxLen
                                         | e - s < maxLen = recGetIndicesToMaxLen (decGreaterThanZero s) e maxLen
                                         | otherwise = (s, e)
        intMaxLineLength = fromInteger $ maxLineLength settings
        (lsi, lei) =
          if trimmedLength > intMaxLineLength
          then recGetIndicesToMaxLen msi mei intMaxLineLength
          else (0, maxLineEndIndex)
        trimWhitespace :: B.ByteString -> B.ByteString
        trimWhitespace = B.drop leadingSpaceCount
        doFormatLine :: B.ByteString -> String
        doFormatLine bs | colorize settings = BC.unpack (colorizeBS msi mei (subByteString lsi lei (trimWhitespace bs)))
                        | otherwise = BC.unpack (subByteString lsi lei (trimWhitespace bs))

colorizeLine :: SearchSettings -> String -> String
colorizeLine settings line =
  case filter (\p -> line =~ p :: Bool) (searchPatterns settings) of
    [] -> line
    (p:_) -> case getAllMatches (line =~ p) :: [(Int, Int)] of
      ((mStart, mLen):_) -> colorizeString line mStart mLen
      [] -> line

formatLine :: SearchSettings -> String -> String
formatLine settings line =
  if colorize settings
  then colorizeLine settings line
  else line

colorizeBSLine :: SearchSettings -> B.ByteString -> B.ByteString
colorizeBSLine settings line =
  case filter (\p -> line =~ p :: Bool) (searchPatterns settings) of
    [] -> line
    (p:_) -> case getAllMatches (line =~ p) :: [(Int, Int)] of
      ((mStart, mLen):_) -> colorizeBS mStart mLen line
      [] -> line

formatBSLine :: SearchSettings -> B.ByteString -> B.ByteString
formatBSLine settings line =
  if colorize settings
  then colorizeBSLine settings line
  else line

formatSingleLine :: SearchSettings -> SearchResult -> String
formatSingleLine settings result =
  formatFilePath (toFindSettings settings) (searchResultPath result) ++ ": " ++
  show (lineNum result) ++ ": [" ++
  show (matchStartIndex result) ++ ":" ++
  show (matchEndIndex result) ++ "]: " ++
  formatMatchingLine settings result

formatMultiLine :: SearchSettings -> SearchResult -> String
formatMultiLine settings result =
  replicate 80 '=' ++ "\n" ++
  formattedFilePath ++ ": " ++
  show (lineNum result) ++ ": [" ++
  show (matchStartIndex result) ++ ":" ++
  show (matchEndIndex result) ++ "]\n" ++
  replicate 80 '-' ++ "\n" ++
  unlines (zipWith (curry formatLine) beforeLineNums resultBeforeLines) ++
  "> " ++ padNumString (show resultLineNum) ++ " | " ++
  (rstrip . BC.unpack) (condColorize (line result)) ++ "\n" ++
  unlines (zipWith (curry formatLine) afterLineNums resultAfterLines)
  where formattedFilePath = formatFilePath (toFindSettings settings) (searchResultPath result)
        resultLineNum = lineNum result
        resultBeforeLines = map (rstrip . BC.unpack) (beforeLines result)
        resultAfterLines = map (rstrip . BC.unpack) (afterLines result)
        maxNumWidth = numWidth (resultLineNum + length resultAfterLines)
        numWidth :: Int -> Int
        numWidth = recNumWidth 1 10
        recNumWidth w m n | n < m = w
                          | otherwise = recNumWidth (w + 1) (m * 10) n
        padNumString ns | length ns < maxNumWidth = padNumString (' ':ns)
                        | otherwise = ns
        firstLineNum = resultLineNum - length resultBeforeLines
        beforeLineNums = [firstLineNum..(resultLineNum-1)]
        afterLineNums = [(resultLineNum+1)..(length resultAfterLines + resultLineNum)]
        formatLine (n,l) = "  " ++ padNumString (show n) ++ " | " ++ l
        condColorize :: B.ByteString -> B.ByteString
        condColorize bs = if colorize settings
                          then colorizeBS (matchStartIndex result - 1) (matchEndIndex result - 1) bs
                          else bs

rstrip :: String -> String
rstrip = reverse . trimLeadingWhitespace . reverse

trimLeadingWhitespace :: String -> String
trimLeadingWhitespace = dropWhile isSpace

compareBySearchResultFields :: SearchResult -> SearchResult -> Ordering
compareBySearchResultFields sr1 sr2 =
  if lineNum sr1 == lineNum sr2
  then
    if matchStartIndex sr1 == matchStartIndex sr2
    then compare (matchEndIndex sr1) (matchEndIndex sr2)
    else compare (matchStartIndex sr1) (matchStartIndex sr2)
  else compare (lineNum sr1) (lineNum sr2)

compareSearchResults :: (FileResult -> FileResult -> Ordering) -> SearchResult -> SearchResult -> Ordering
compareSearchResults compareFileResults sr1 sr2 =
  if frcmp == EQ
  then compareBySearchResultFields sr1 sr2
  else frcmp
  where fr1 = fileResult sr1
        fr2 = fileResult sr2
        frcmp = compareFileResults fr1 fr2

getCompareSearchResultsFunc :: SearchSettings -> SearchResult -> SearchResult -> Ordering
getCompareSearchResultsFunc settings =
  compareSearchResults (getCompareFileResultsFunc (toFindSettings settings))

doSortBySearchResults :: SearchSettings -> [SearchResult] -> [SearchResult]
doSortBySearchResults settings = sortBy $ getCompareSearchResultsFunc settings

sortSearchResults :: SearchSettings -> [SearchResult] -> [SearchResult]
sortSearchResults settings searchResults =
  if sortDescending settings
  then reverse $ doSortBySearchResults settings searchResults
  else doSortBySearchResults settings searchResults
