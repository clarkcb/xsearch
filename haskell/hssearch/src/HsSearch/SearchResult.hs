module HsSearch.SearchResult
  ( SearchResult(..)
  , blankSearchResult
  , formatBS
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

import HsFind.ConsoleColor (Color(..), colorizeString, colorToConsoleColor, consoleGreen, consoleReset)
import HsFind.FileResult (FileResult(..), blankFileResult, formatFilePath, getCompareFileResultsFunc)
import HsFind.StringUtil (trimLeadingWhitespace, trimTrailingWhitespace)
import HsSearch.ByteStringUtil
import HsSearch.SearchSettings
import qualified Data.ByteString as B

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

colorizeBS :: B.ByteString -> Int -> Int -> B.ByteString
colorizeBS bs startIdx len =
  BC.concat [sliceByteString 0 startIdx bs,
             BC.pack consoleGreen,
             sliceByteString startIdx endIdx bs,
             BC.pack consoleReset,
             sliceByteString endIdx (BC.length bs) bs]
  where endIdx = startIdx + len

formatBS :: SearchSettings -> B.ByteString -> B.ByteString
formatBS settings bs =
  if colorize settings
  then colorizeBS bs 0 (BC.length bs)
  else bs

formatResultMatch :: SearchSettings -> SearchResult -> String
formatResultMatch settings result =
  doFormatMatch l
  where
    l = line result
    matchStartIdx = matchStartIndex result - 1
    matchEndIdx = matchEndIndex result - 1
    matchLength = matchEndIdx - matchStartIdx
    intMaxLineLength = if maxLineLength settings < 0 then matchLength + 1 else fromInteger $ maxLineLength settings
    -- maxLimit = intMaxLineLength /= 0
    trunc = matchLength > intMaxLineLength
    prefix = if trunc && matchStartIdx > 2 then BC.pack "..." else BC.pack ""
    suffix = if trunc then BC.pack "..." else BC.pack ""
    colorStartIdx = if trunc then B.length prefix else 0
    colorEndIdx = if trunc then intMaxLineLength - B.length suffix else matchLength
    -- colorLength = colorEndIdx - colorStartIdx
    matchEndIdx' = if trunc then matchStartIdx + colorEndIdx else matchEndIdx
    matchStartIdx' = if trunc then matchStartIdx + colorStartIdx else matchStartIdx
    matchString = B.concat [prefix, sliceByteString matchStartIdx' matchEndIdx' l, suffix]
    doFormatMatch :: B.ByteString -> String
    doFormatMatch bs | BC.length bs == 0 = ""
                     | intMaxLineLength == 0 = ""
                    --  | colorize settings = BC.unpack (colorizeBS matchString colorStartIdx (fromIntegral colorLength))
                     | colorize settings = BC.unpack (colorizeBS matchString colorStartIdx (colorEndIdx - colorStartIdx))
                     | otherwise = BC.unpack matchString

getIndicesForStringAndMaxLength :: B.ByteString -> Int -> Int -> Int -> (Int, Int, Int, Int)
getIndicesForStringAndMaxLength bs matchStartIdx matchEndIdx maxLength
  | bsLength == 0 = (0, 0, 0, 0)
  | maxLength == 0 = (0, 0, 0, 0)
  | maxLength < 0 = getIndicesForStringAndMaxLength bs matchStartIdx matchEndIdx $ bsLength + 1
  | trimmedLength == 0 = (0, 0, 0, 0)
  | trimmedLength <= maxLength = (lineStartIdx, lineEndIdx + 2, matchStartIdx', matchEndIdx')
  | otherwise = (lsi, lei, msi, mei)
  where
    bsLength = BC.length bs
    lineStartIdx = leftWhitespaceByteString bs
    lineEndIdx = bsLength - 1 - rightWhitespaceByteString bs
    trimmedLength = lineEndIdx - lineStartIdx
    matchLength = matchEndIdx - matchStartIdx
    matchStartIdx' = matchStartIdx - lineStartIdx
    matchEndIdx' = matchStartIdx' + matchLength
    decGreaterThanZero :: Int -> Int
    decGreaterThanZero n | n > 0 = n - 1
                          | otherwise = n
    incLessThanMax :: Int -> Int -> Int
    incLessThanMax n maxN | n < maxN = n + 1
                          | otherwise = n
    recGetIndicesToMaxLen :: Int -> Int -> Int -> (Int, Int)
    recGetIndicesToMaxLen si ei maxLen
      | ei - si + 1 < maxLen = recGetIndicesToMaxLen (decGreaterThanZero si) (incLessThanMax ei trimmedLength) maxLen
      | ei - si < maxLen = recGetIndicesToMaxLen (decGreaterThanZero si) ei maxLen
      | otherwise = (si, ei)
    (lsi, lei) =
      if trimmedLength > maxLength
      then recGetIndicesToMaxLen matchStartIdx' matchEndIdx' maxLength
      else (0, trimmedLength + 2)
    msi = matchStartIdx - lsi
    mei = msi + matchLength

formatResultLineWithMatch :: SearchSettings -> SearchResult -> String
formatResultLineWithMatch settings result =
  doFormatLine l
  where
    l = line result
    lineLength = BC.length l
    matchStartIdx = matchStartIndex result - 1
    matchEndIdx = matchEndIndex result - 1
    matchLength = matchEndIdx - matchStartIdx
    intMaxLineLength = if maxLineLength settings < 0 then lineLength + 1 else fromInteger $ maxLineLength settings
    maxLimit = intMaxLineLength > 0
    (lsi, lei, msi, mei) = getIndicesForStringAndMaxLength l matchStartIdx matchEndIdx intMaxLineLength
    prefix = if maxLimit && lsi > 2 then BC.pack "..." else BC.pack ""
    suffix = if maxLimit && lei < (lineLength - 2) then BC.pack "..." else BC.pack ""
    lsi' = lsi + BC.length prefix
    lei' = lei - BC.length suffix
    trimSliceByteString :: Int -> Int -> B.ByteString -> B.ByteString
    trimSliceByteString si ei bs = B.concat [prefix, sliceByteString si ei bs, suffix]
    doFormatLine :: B.ByteString -> String
    doFormatLine bs | BC.length bs == 0 = ""
                    | intMaxLineLength == 0 = ""
                    | lei - lsi == 0 = ""
                    | colorize settings = BC.unpack (colorizeBS (trimSliceByteString lsi' lei' bs) msi matchLength)
                    | otherwise = BC.unpack (trimSliceByteString lsi' lei' bs)

formatResultLine :: SearchSettings -> SearchResult -> String
formatResultLine settings result
  | BC.length (trimByteString (line result)) == 0 = ""
  | fromInteger (maxLineLength settings) == 0 = ""
  | otherwise =
      if matchLength > intMaxLineLength
      then formatResultMatch settings result
      else formatResultLineWithMatch settings result
      where
        matchLength = matchEndIndex result - matchStartIndex result
        intMaxLineLength =
          case fromInteger (maxLineLength settings) of
            0 -> 0
            i | i < 0 -> matchLength + 1
            i -> i

colorizeLine :: SearchSettings -> String -> String
colorizeLine settings line =
  case filter (\p -> line =~ p :: Bool) (searchPatterns settings) of
    [] -> line
    (p:_) -> case getAllMatches (line =~ p) :: [(Int, Int)] of
      ((mStart, mLen):_) -> colorizeString line mStart mLen (lineColor settings)
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
      ((mStart, mLen):_) -> colorizeBS line mStart mLen
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
  formatResultLine settings result

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
  (trimTrailingWhitespace . BC.unpack) (condColorize (line result)) ++ "\n" ++
  unlines (zipWith (curry formatLine) afterLineNums resultAfterLines)
  where formattedFilePath = formatFilePath (toFindSettings settings) (searchResultPath result)
        resultLineNum = lineNum result
        resultBeforeLines = map (trimTrailingWhitespace . BC.unpack) (beforeLines result)
        resultAfterLines = map (trimTrailingWhitespace . BC.unpack) (afterLines result)
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
                          then colorizeBS bs (matchStartIndex result - 1) (matchEndIndex result - 1)
                          else bs

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
