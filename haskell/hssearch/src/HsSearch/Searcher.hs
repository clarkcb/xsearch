module HsSearch.Searcher
    (
      doSearch
    , doSearchFiles
    , formatMatchingDirs
    , formatMatchingFiles
    , formatMatchingLines
    , formatResults
    , getSearchFiles
    , searchContents
    , searchLines
    , validateSearchSettings
    ) where

-- import Control.Monad (forM)
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BC
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BC
import Data.Char (isSpace, toUpper)
import Data.List (nub, sort, sortBy)
import Data.Maybe (catMaybes)
import System.FilePath (takeDirectory)
import Text.Regex.PCRE

import HsFind.FileResult
import HsFind.FileTypes
import HsFind.FileUtil
import HsFind.Finder (doFind, validateFindSettings)

import HsSearch.SearchResult
import HsSearch.SearchSettings


validateSearchSettings :: SearchSettings -> Maybe String
validateSearchSettings settings =
  if printUsage settings
    then Nothing
    else
      case validateFindSettings $ toFindSettings settings of
        Just err -> Just err
        Nothing -> recValidateSettings validators []
  where recValidateSettings :: [SearchSettings -> [String]] -> [String] -> Maybe String
        recValidateSettings validators' errs = do
          case errs of
            [] -> case validators' of
                    [] -> Nothing
                    (v:vs) -> recValidateSettings vs (v settings)
            _ -> Just $ head errs
        validators = [ \s -> ["No search patterns defined" | null (searchPatterns s)]
                     , \s -> ["Invalid lines after" | linesAfter s < 0]
                     , \s -> ["Invalid lines before" | linesBefore s < 0]
                     , \s -> ["Invalid max line length" | maxLineLength s < 0]
                     , \s -> ["Invalid max size" | maxSize s < 0]
                     , \s -> ["Invalid min size" | minSize s < 0]
                     ]

getSearchFiles :: SearchSettings -> IO (Either String [FileResult])
getSearchFiles settings = do
  doFind $ toFindSettings settings

searchBinaryFile :: SearchSettings -> FilePath -> IO [SearchResult]
searchBinaryFile settings f = do
  blobEither <- getFileByteString f
  case blobEither of
    (Left _) -> return [] -- todo: figure out to relay error
    (Right blob) -> return $ addFilePath (searchBlob settings blob)
  where addFilePath = map (\r -> r {filePath=f})

searchBlob :: SearchSettings -> B.ByteString -> [SearchResult]
searchBlob settings blob =
  concatMap (searchBlobForPattern settings blob) (searchPatterns settings)

searchBlobForPattern :: SearchSettings -> B.ByteString -> String -> [SearchResult]
searchBlobForPattern settings blob = patternResults
  where lineMatchIndices :: String -> [(Int,Int)]
        lineMatchIndices p = if firstMatch settings
                               then take 1 $ matchIndices blob p
                               else matchIndices blob p
        patternResults :: String -> [SearchResult]
        patternResults p = map (resultFromPatternMatchIndices p) (lineMatchIndices p)
        resultFromPatternMatchIndices :: String -> (Int, Int) -> SearchResult
        resultFromPatternMatchIndices p ix =
          blankSearchResult { searchPattern=p
                            , lineNum=0
                            , matchStartIndex=fst ix + 1
                            , matchEndIndex=snd ix + 1
                            , line=B.empty
                            }

searchTextFile :: SearchSettings -> FilePath -> IO [SearchResult]
searchTextFile settings f =
  if multiLineSearch settings
    then searchTextFileContents settings f
    else searchTextFileLines settings f

matchOffsetsAndLengths :: B.ByteString -> String -> [(MatchOffset,MatchLength)]
matchOffsetsAndLengths s p = getAllMatches $ s =~ p :: [(MatchOffset,MatchLength)]

matchIndices :: B.ByteString -> String -> [(Int,Int)]
matchIndices s p = map (\(x,y) -> (x, x+y)) (matchOffsetsAndLengths s p)

linesMatch :: [B.ByteString] -> [String] -> [String] -> Bool
linesMatch [] _ _ = False
linesMatch _ [] [] = True
linesMatch ls lsInPatterns lsOutPatterns = inPatternMatches && not outPatternMatches
  where inPatternMatches = null lsInPatterns || anyMatchesAnyPattern ls lsInPatterns
        outPatternMatches = not (null lsOutPatterns) && anyMatchesAnyPattern ls lsOutPatterns

anyMatchesAnyPattern :: [B.ByteString] -> [String] -> Bool
anyMatchesAnyPattern ls = any (anyMatchesPattern ls)

matchesPattern :: B.ByteString -> String -> Bool
matchesPattern l p = l =~ p

anyMatchesPattern :: [B.ByteString] -> String -> Bool
anyMatchesPattern ls p = any (`matchesPattern` p) ls

matchesAnyPattern :: B.ByteString -> [String] -> Bool
matchesAnyPattern l = any (matchesPattern l)

searchTextFileContents :: SearchSettings -> FilePath -> IO [SearchResult]
searchTextFileContents settings f = do
  contentsEither <- getFileByteString f
  case contentsEither of
    (Left _) -> return [] -- todo: figure out to relay error
    (Right contents) -> return $ addFilePath (searchContents settings contents)
  where addFilePath = map (\r -> r {filePath=f})

searchContents :: SearchSettings -> B.ByteString -> [SearchResult]
searchContents settings contents =
  concatMap (searchContentsForPattern settings contents) (searchPatterns settings)

searchContentsForPattern :: SearchSettings -> B.ByteString -> String -> [SearchResult]
searchContentsForPattern settings contents = patternResults
  where patternResults p = catMaybes (maybePatternResults p)
        maybePatternResults p = map (maybeResultFromPatternMatchIndices p) (firstOrAllIndices p)
        firstOrAllIndices p = if firstMatch settings
                              then take 1 (matchIndices contents p)
                              else matchIndices contents p
        newlineIndices =  BC.findIndices (=='\n') contents
        startLineIndices = 0 : map (+1) newlineIndices
        startLineIndex idx = case takeWhile (<=idx) startLineIndices of
                             [] -> 0
                             x  -> last x
        endLineIndex idx   = case dropWhile (<=idx) startLineIndices of
                             [] -> BC.length contents - 1
                             x  -> head x - 1
        lineLength i = endLineIndex i - startLineIndex i
        lineAtIndex i = B.take (lineLength i) $ B.drop (startLineIndex i) contents
        countNewlines s = BC.length $ BC.filter (=='\n') s
        intLinesBefore = fromInteger $ linesBefore settings
        takeRight n = reverse . take n . reverse
        linesBeforeIndices i n = (takeRight n . takeWhile (<i)) startLineIndices
        getLinesBefore i n = map lineAtIndex (linesBeforeIndices i n)
        beforeLns i | intLinesBefore == 0 = []
                    | otherwise = getLinesBefore (startLineIndex i) intLinesBefore
        intLinesAfter = fromInteger $ linesAfter settings
        linesAfterIndices i n = (take n . dropWhile (<=i)) startLineIndices
        getLinesAfter i n = map lineAtIndex (linesAfterIndices i n)
        afterLns i | intLinesAfter == 0 = []
                   | otherwise = getLinesAfter (startLineIndex i) intLinesAfter
        checkBefore = linesBefore settings > 0
        beforeLinesMatch bs =
          not checkBefore
          || linesMatch bs (inLinesBeforePatterns settings) (outLinesBeforePatterns settings)
        checkAfter = linesAfter settings > 0
        afterLinesMatch as =
          not checkAfter
          || linesMatch as (inLinesAfterPatterns settings) (outLinesAfterPatterns settings)
        maybeResultFromPatternMatchIndices :: String -> (Int, Int) -> Maybe SearchResult
        maybeResultFromPatternMatchIndices p ix =
          if beforeLinesMatch bs && afterLinesMatch as
          then
            Just blankSearchResult { searchPattern=p
                                   , lineNum=lineCount
                                   , matchStartIndex=msi
                                   , matchEndIndex=mei
                                   , line=lineAtIndex (fst ix)
                                   , beforeLines=bs
                                   , afterLines=as
                                   }
          else Nothing
          where lineCount = countNewlines (B.take (fst ix) contents) + 1
                sli = startLineIndex (fst ix)
                msi = fst ix - sli + 1
                mei = snd ix - sli + 1
                bs = beforeLns (fst ix)
                as = afterLns (fst ix)

searchTextFileLines :: SearchSettings -> FilePath -> IO [SearchResult]
searchTextFileLines settings f = do
  fileLinesEither <- getFileLines f
  case fileLinesEither of
    (Left _) -> return [] -- todo: figure out to relay error
    (Right fileLines) -> return $ addFilePath (searchLines settings fileLines)
  where addFilePath = map (\r -> r {filePath=f})

searchLines :: SearchSettings -> [B.ByteString] -> [SearchResult]
searchLines settings lineList = recSearchLines settings [] lineList 0 []

recSearchLines :: SearchSettings -> [B.ByteString] -> [B.ByteString] -> Int -> [SearchResult] -> [SearchResult]
recSearchLines settings beforeList lst num results =
  case lst of
    []     -> results
    (l:ls) -> recSearchLines settings (newBefore l) ls (num + 1) (updatedResults l)
  where intLinesBefore = fromInteger $ linesBefore settings
        newBefore l | intLinesBefore == 0 = []
                    | length beforeList == intLinesBefore = tail beforeList ++ [l]
                    | otherwise = beforeList ++ [l]
        intLinesAfter = fromInteger $ linesAfter settings
        afterToPatterns = linesAfterToPatterns settings
        afterUntilPatterns = linesAfterUntilPatterns settings
        checkAfterTo = not (null afterToPatterns)
        checkAfterUntil = not (null afterUntilPatterns)
        notMatchesAnyPattern ps l = not $ matchesAnyPattern l ps
        afterToCount =
          if checkAfterTo
          then length (takeWhile (notMatchesAnyPattern afterToPatterns) (tail lst)) + 1
          else 0
        afterUntilCount =
          if checkAfterUntil
          then length (takeWhile (notMatchesAnyPattern afterUntilPatterns) (tail lst))
          else 0
        afterList
          | checkAfterTo = take afterToCount (tail lst)
          | checkAfterUntil = take afterUntilCount (tail lst)
          | otherwise = take intLinesAfter (tail lst)
        updatedResults l = results ++ newResults l
        newResults l = concatMap (searchNextPattern l) filteredPatterns
        searchNextPattern l = searchLineForPattern settings (num + 1) beforeList l afterList
        filteredPatterns = if firstMatch settings
                           then filter firstMatchNotMet patterns
                           else patterns
        firstMatchNotMet p = not (any (\r -> searchPattern r == p) results)
        patterns = searchPatterns settings

searchLineForPattern :: SearchSettings -> Int -> [B.ByteString] -> B.ByteString -> [B.ByteString] -> String -> [SearchResult]
searchLineForPattern settings num bs l as = patternResults
  where checkBefore = linesBefore settings > 0
        beforeLinesMatch =
          not checkBefore
          || linesMatch bs (inLinesBeforePatterns settings) (outLinesBeforePatterns settings)
        checkAfter = linesAfter settings > 0
        afterLinesMatch =
          not checkAfter
          || linesMatch as (inLinesAfterPatterns settings) (outLinesAfterPatterns settings)
        lineMatchIndices :: String -> [(Int,Int)]
        lineMatchIndices p = if beforeLinesMatch && afterLinesMatch
                             then if firstMatch settings
                                  then take 1 $ matchIndices l p
                                  else matchIndices l p
                             else []
        patternResults :: String -> [SearchResult]
        patternResults p = map (resultFromPatternMatchIndices p) (lineMatchIndices p)
        resultFromPatternMatchIndices :: String -> (Int, Int) -> SearchResult
        resultFromPatternMatchIndices p ix =
          blankSearchResult { searchPattern=p
                            , lineNum=num
                            , matchStartIndex=fst ix + 1
                            , matchEndIndex=snd ix + 1
                            , line=l
                            , beforeLines=bs
                            , afterLines=as
                            }

doSearchFile :: SearchSettings -> FileResult -> IO [SearchResult]
doSearchFile settings fr =
  case fileResultType fr of
    Binary -> searchBinaryFile settings $ fileResultPath fr
    filetype | filetype `elem` [Code, Text, Xml] -> searchTextFile settings $ fileResultPath fr
    _ -> return []

doSearchFiles :: SearchSettings -> [FileResult] -> IO [SearchResult]
doSearchFiles settings files = do
  results <- mapM (doSearchFile settings) files
  return $ concat results

doSearch :: SearchSettings -> IO (Either String [SearchResult])
doSearch settings = do
  findResultsEither <- doFind $ toFindSettings settings
  case findResultsEither of
    Left err -> return $ Left err
    Right fileResults -> do
      searchResults <- doSearchFiles settings fileResults
      return $ Right searchResults

formatResults :: SearchSettings -> [SearchResult] -> String
formatResults settings results =
  if not (null results) then
    "\nSearch results (" ++ show (length results) ++ "):\n" ++
    unlines (map (formatSearchResult settings) results)
  else "\nSearch results: 0\n"

getMatchingDirs :: [SearchResult] -> [FilePath]
getMatchingDirs = sort . nub . map getDirectory
  where getDirectory r = takeDirectory (filePath r)

formatMatchingDirs :: SearchSettings -> [SearchResult] -> String
formatMatchingDirs settings results = 
  if not (null matchingDirs) then
    "\nMatching directories (" ++ show (length matchingDirs) ++ "):\n" ++
    unlines matchingDirs
  else "\nMatching directories: 0\n"
  where findSettings = toFindSettings settings
        matchingDirs = map (formatDirectory findSettings) $ getMatchingDirs results

getMatchingFiles :: [SearchResult] -> [FilePath]
getMatchingFiles = sort . nub . map filePath

formatMatchingFiles :: SearchSettings -> [SearchResult] -> String
formatMatchingFiles settings results = 
  if not (null matchingFiles) then
    "\nMatching files (" ++ show (length matchingFiles) ++ "):\n" ++
    unlines matchingFiles
  else "\nMatching files: 0\n"
  where findSettings = toFindSettings settings
        matchingFiles = map (formatFilePath findSettings) $ getMatchingFiles results

byteStringToUpper :: B.ByteString -> B.ByteString
byteStringToUpper = BC.pack . map toUpper . BC.unpack

doSortCaseInsensitive :: [B.ByteString] -> [B.ByteString]
doSortCaseInsensitive = sortBy compareCaseInsensitive
  where compareCaseInsensitive a b = byteStringToUpper a `compare` byteStringToUpper b

getMatchingLines :: SearchSettings -> [SearchResult] -> [B.ByteString]
getMatchingLines settings results | unique = (doSortCaseInsensitive . nub . map trimLine) results
                                  | otherwise = (doSortCaseInsensitive . map trimLine) results
  where unique = uniqueLines settings
        trimLine = BC.dropWhile isSpace . line

formatMatchingLines :: SearchSettings -> [SearchResult] -> String
formatMatchingLines settings results = 
  "\n" ++ hdrText ++ " (" ++ show (length matchingLines) ++ "):\n" ++
  BC.unpack (BC.intercalate (BC.pack "\n") matchingLines) ++ "\n"
  where matchingLines = map (formatBSLine settings) $ getMatchingLines settings results
        hdrText = if uniqueLines settings
                  then "Unique lines with matches"
                  else "Lines with matches"
