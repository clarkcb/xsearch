module HsSearch.Searcher
    (
      doSearch
    , doSearchFiles
    , formatSearchResultMatchingDirs
    , formatSearchResultMatchingFiles
    , formatSearchResultMatchingLines
    , formatSearchResultMatches
    , formatSearchResults
    , getSearcher
    , getSearchFiles
    , ioValidateSearchSettings
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
import HsFind.Finder (doFind, formatMatchingDirs, formatMatchingFiles, getFinder, ioValidateFindSettings, validateFindSettings)

import HsSearch.ByteStringUtil (sliceByteString, trimLeftByteString, trimRightByteString)
import HsSearch.SearchConfig
import HsSearch.SearchResult
import HsSearch.SearchSettings


data Searcher = Searcher
  { config:: SearchConfig
  , settings :: SearchSettings
  }

getSearcher :: SearchConfig -> SearchSettings -> Searcher
getSearcher config settings = Searcher
  { config = config
  , settings = settings
  }

validateSearchSettings :: SearchSettings -> Maybe String
validateSearchSettings settings =
  if printUsage settings
    then Nothing
    else
      case validateFindSettings $ toFindSettings settings of
        Just err -> Just err
        Nothing -> recValidateSearchSettings settings validators []
  where recValidateSearchSettings :: SearchSettings -> [SearchSettings -> [String]] -> [String] -> Maybe String
        recValidateSearchSettings settings' validators' errs = do
          case errs of
            [] -> case validators' of
                    [] -> Nothing
                    (v:vs) -> recValidateSearchSettings settings' vs (v settings')
            _ -> Just $ head errs
        validators = [ \s -> ["No search patterns defined" | null (searchPatterns s)]
                     , \s -> ["Invalid lines after" | linesAfter s < 0]
                     , \s -> ["Invalid lines before" | linesBefore s < 0]
                    --  , \s -> ["Invalid max size" | maxSize s < 0]
                    --  , \s -> ["Invalid min size" | minSize s < 0]
                     ]

-- This function adds validation for things like path existence that require IO
ioValidateSearchSettings :: SearchSettings -> IO (Maybe String)
ioValidateSearchSettings settings =
  case validateSearchSettings settings of
    Just err -> return $ Just err
    Nothing -> do
      ioFindErrs <- ioValidateFindSettings $ toFindSettings settings
      case ioFindErrs of
        Just err -> return $ Just err
        Nothing -> recIoValidateSearchSettings settings validators []
  where recIoValidateSearchSettings :: SearchSettings -> [SearchSettings -> IO [String]] -> [String] -> IO (Maybe String)
        recIoValidateSearchSettings settings' validators' errs = do
          case errs of
            [] -> case validators' of
                    [] -> return Nothing
                    (v:vs) -> do
                      newErrs <- v settings'
                      recIoValidateSearchSettings settings' vs newErrs
            _ -> return $ Just $ head errs
        -- currently no search-specific IO validators
        validators = []

getSearchFiles :: Searcher -> IO (Either String [FileResult])
getSearchFiles searcher = do
  doFind $ getFinder (findConfig (config searcher)) (toFindSettings (settings searcher))

searchBinaryFile :: Searcher -> FileResult -> IO [SearchResult]
searchBinaryFile searcher fr = do
  blobEither <- getFileByteString $ fileResultPath fr
  case blobEither of
    (Left _) -> return [] -- todo: figure out to relay error
    (Right blob) -> return $ addFileResult (searchBlob searcher blob)
  where addFileResult = map (\r -> r {fileResult=fr})

searchBlob :: Searcher -> B.ByteString -> [SearchResult]
searchBlob searcher blob =
  concatMap (searchBlobForPattern searcher blob) (searchPatterns (settings searcher))

searchBlobForPattern :: Searcher -> B.ByteString -> String -> [SearchResult]
searchBlobForPattern searcher blob = patternResults
  where lineMatchIndices :: String -> [(Int,Int)]
        lineMatchIndices p = if firstMatch (settings searcher)
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

searchTextFile :: Searcher -> FileResult -> IO [SearchResult]
searchTextFile searcher fr =
  if multiLineSearch (settings searcher)
    then searchTextFileContents searcher fr
    else searchTextFileLines searcher fr

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

searchTextFileContents :: Searcher -> FileResult -> IO [SearchResult]
searchTextFileContents searcher fr = do
  contentsEither <- getFileByteString $ fileResultPath fr
  case contentsEither of
    (Left _) -> return [] -- todo: figure out to relay error
    (Right contents) -> return $ addFileResult (searchContents searcher contents)
  where addFileResult = map (\r -> r {fileResult=fr})

searchContents :: Searcher -> B.ByteString -> [SearchResult]
searchContents searcher contents =
  concatMap (searchContentsForPattern searcher contents) (searchPatterns (settings searcher))

searchContentsForPattern :: Searcher -> B.ByteString -> String -> [SearchResult]
searchContentsForPattern searcher contents = patternResults
  where patternResults p = catMaybes (maybePatternResults p)
        maybePatternResults p = map (maybeResultFromPatternMatchIndices p) (firstOrAllIndices p)
        firstOrAllIndices p = if firstMatch (settings searcher)
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
        intLinesBefore = fromInteger $ linesBefore (settings searcher)
        takeRight n = reverse . take n . reverse
        linesBeforeIndices i n = (takeRight n . takeWhile (<i)) startLineIndices
        getLinesBefore i n = map lineAtIndex (linesBeforeIndices i n)
        beforeLns i | intLinesBefore == 0 = []
                    | otherwise = getLinesBefore (startLineIndex i) intLinesBefore
        intLinesAfter = fromInteger $ linesAfter (settings searcher)
        linesAfterIndices i n = (take n . dropWhile (<=i)) startLineIndices
        getLinesAfter i n = map lineAtIndex (linesAfterIndices i n)
        afterLns i | intLinesAfter == 0 = []
                   | otherwise = getLinesAfter (startLineIndex i) intLinesAfter
        checkBefore = linesBefore (settings searcher) > 0
        beforeLinesMatch bs =
          not checkBefore
          || linesMatch bs (inLinesBeforePatterns (settings searcher)) (outLinesBeforePatterns (settings searcher))
        checkAfter = linesAfter (settings searcher) > 0
        afterLinesMatch as =
          not checkAfter
          || linesMatch as (inLinesAfterPatterns (settings searcher)) (outLinesAfterPatterns (settings searcher))
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

searchTextFileLines :: Searcher -> FileResult -> IO [SearchResult]
searchTextFileLines searcher fr = do
  fileLinesEither <- getFileLines $ fileResultPath fr
  case fileLinesEither of
    (Left _) -> return [] -- todo: figure out to relay error
    (Right fileLines) -> return $ addFileResult (searchLines searcher fileLines)
  where addFileResult = map (\r -> r {fileResult=fr})

searchLines :: Searcher -> [B.ByteString] -> [SearchResult]
searchLines searcher lineList = recSearchLines searcher [] lineList 0 []

recSearchLines :: Searcher -> [B.ByteString] -> [B.ByteString] -> Int -> [SearchResult] -> [SearchResult]
recSearchLines searcher beforeList lst num results =
  case lst of
    []     -> results
    (l:ls) -> recSearchLines searcher (newBefore l) ls (num + 1) (updatedResults l)
  where intLinesBefore = fromInteger $ linesBefore (settings searcher)
        newBefore l | intLinesBefore == 0 = []
                    | length beforeList == intLinesBefore = tail beforeList ++ [l]
                    | otherwise = beforeList ++ [l]
        intLinesAfter = fromInteger $ linesAfter (settings searcher)
        afterToPatterns = linesAfterToPatterns (settings searcher)
        afterUntilPatterns = linesAfterUntilPatterns (settings searcher)
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
        searchNextPattern l = searchLineForPattern searcher (num + 1) beforeList l afterList
        filteredPatterns = if firstMatch (settings searcher)
                           then filter firstMatchNotMet patterns
                           else patterns
        firstMatchNotMet p = not (any (\r -> searchPattern r == p) results)
        patterns = searchPatterns (settings searcher)

searchLineForPattern :: Searcher -> Int -> [B.ByteString] -> B.ByteString -> [B.ByteString] -> String -> [SearchResult]
searchLineForPattern searcher num bs l as = patternResults
  where checkBefore = linesBefore (settings searcher) > 0
        beforeLinesMatch =
          not checkBefore
          || linesMatch bs (inLinesBeforePatterns (settings searcher)) (outLinesBeforePatterns (settings searcher))
        checkAfter = linesAfter (settings searcher) > 0
        afterLinesMatch =
          not checkAfter
          || linesMatch as (inLinesAfterPatterns (settings searcher)) (outLinesAfterPatterns (settings searcher))
        lineMatchIndices :: String -> [(Int,Int)]
        lineMatchIndices p = if beforeLinesMatch && afterLinesMatch
                             then if firstMatch (settings searcher)
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

doSearchFile :: Searcher -> FileResult -> IO [SearchResult]
doSearchFile searcher fr =
  case fileResultType fr of
    Binary -> searchBinaryFile searcher fr
    filetype | filetype `elem` [Code, Text, Xml] -> searchTextFile searcher fr
    _ -> return []

doSearchFiles :: Searcher -> [FileResult] -> IO [SearchResult]
doSearchFiles searcher files = do
  results <- mapM (doSearchFile searcher) files
  return $ concat results

doSearch :: Searcher -> IO (Either String [SearchResult])
doSearch searcher = do
  findResultsEither <- getSearchFiles searcher
  case findResultsEither of
    Left err -> return $ Left err
    Right fileResults -> do
      searchResults <- doSearchFiles searcher fileResults
      return $ Right $ sortSearchResults (settings searcher) searchResults

formatSearchResults :: SearchSettings -> [SearchResult] -> String
formatSearchResults settings results =
  if not (null results) then
    "\nSearch results (" ++ show (length results) ++ "):\n" ++
    unlines (map (formatSearchResult settings) results)
  else "\nSearch results: 0\n"

getMatchingDirs :: [SearchResult] -> [FilePath]
getMatchingDirs = sort . nub . map getDirectory
  where getDirectory r = takeDirectory $ searchResultPath r

formatSearchResultMatchingDirs :: SearchSettings -> [SearchResult] -> String
formatSearchResultMatchingDirs settings results = 
  if not (null matchingDirs) then
    "\nMatching directories (" ++ show (length matchingDirs) ++ "):\n" ++
    unlines matchingDirs
  else "\nMatching directories: 0\n"
  where findSettings = toFindSettings settings
        matchingDirs = map (formatDirectory findSettings) $ getMatchingDirs results

getMatchingFiles :: [SearchResult] -> [FilePath]
getMatchingFiles = sort . nub . map searchResultPath

formatSearchResultMatchingFiles :: SearchSettings -> [SearchResult] -> String
formatSearchResultMatchingFiles settings results = 
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
getMatchingLines settings results | unique = (doSort . nub . map trimLine) matchResults
                                  | otherwise = (doSort . map trimLine) matchResults
  where unique = uniqueLines settings
        doSort = if sortCaseInsensitive settings
                   then doSortCaseInsensitive
                   else sort
        trimLine = trimLeftByteString . line
        matchResults = filter (\r -> lineNum r > 0) results

formatSearchResultMatchingLines :: SearchSettings -> [SearchResult] -> String
formatSearchResultMatchingLines settings results = 
  "\n" ++ hdrText ++ " (" ++ show (length matchingLines) ++ "):\n" ++
  BC.unpack (BC.intercalate (BC.pack "\n") matchingLines) ++ "\n"
  where matchingLines = map (formatBSLine settings) $ getMatchingLines settings results
        hdrText = if uniqueLines settings
                  then "Unique matching lines"
                  else "Matching lines"

getMatches :: SearchSettings -> [SearchResult] -> [B.ByteString]
getMatches settings results | unique = (doSort . nub . map getMatchString) matchResults
                            | otherwise = (doSort . map getMatchString) matchResults
  where unique = uniqueLines settings
        doSort = if sortCaseInsensitive settings
                   then doSortCaseInsensitive
                   else sort
        matchResults = filter (\r -> lineNum r > 0) results
        getMatchString r =
          let l = line r
              msi = matchStartIndex r - 1
              mei = matchEndIndex r - 1
              m = sliceByteString msi mei l
          in m

formatSearchResultMatches :: SearchSettings -> [SearchResult] -> String
formatSearchResultMatches settings results = 
  "\n" ++ hdrText ++ " (" ++ show (length matches) ++ "):\n" ++
  BC.unpack (BC.intercalate (BC.pack "\n") matches) ++ "\n"
  where matches = map (formatBS settings) $ getMatches settings results
        hdrText = if uniqueLines settings
                  then "Unique matches"
                  else "Matches"
