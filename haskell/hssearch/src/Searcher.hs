module Searcher
    (
      doSearch
    , doSearchFiles
    , getSearchDirs
    , getSearchFiles
    ) where

import Control.Monad (liftM)
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BC
import Data.Maybe (catMaybes)
import Text.Regex.PCRE

import FileTypes
import FileUtility
import SearchResult
import SearchSettings


-- TODO: use this type with all file-based functions
data SearchFile = SearchFile {
                                searchFilePath :: FilePath
                              , searchFileType :: FileType
                              } deriving (Show, Eq)


isSearchDir :: SearchSettings -> FilePath -> Bool
isSearchDir settings d = and $ map ($d) tests
  where tests :: [FilePath -> Bool]
        tests = [ (\x -> length inPatterns == 0
                         || any (\p -> x =~ p :: Bool) inPatterns)
                , (\x -> length outPatterns == 0
                         || all (\p -> not $ x =~ p :: Bool) outPatterns)
                , (\x -> not (isHiddenFilePath x) || includeHidden)
                ]
        inPatterns = inDirPatterns settings
        outPatterns = outDirPatterns settings
        includeHidden = not $ excludeHidden settings

getSearchDirs :: SearchSettings -> IO [FilePath]
getSearchDirs settings = do
  dirs <- getRecursiveDirectories $ startPath settings
  let searchDirs = filter (isSearchDir settings) $ [startPath settings] ++ dirs
  return searchDirs

isSearchFile :: SearchSettings -> FilePath -> Bool
isSearchFile settings fp = and $ map ($fp) tests
  where tests :: [FilePath -> Bool]
        tests = [ (\x -> length inExts == 0
                         || hasInExt x)
                , (\x -> length outExts == 0
                         || not (hasOutExt x))
                , (\x -> length inPatterns == 0
                         || any (\p -> x =~ p :: Bool) inPatterns)
                , (\x -> length outPatterns == 0
                         || all (\p -> not $ x =~ p :: Bool) outPatterns)
                , (\x -> not (isHiddenFilePath x) || includeHidden)
                ]
        inExts = inExtensions settings
        hasInExt f = case getExtension f of
                       Just x -> x `elem` inExts
                       Nothing -> null inExts
        outExts = outExtensions settings
        hasOutExt f = case getExtension f of
                        Just x -> x `elem` outExts
                        Nothing -> False
        inPatterns = inFilePatterns settings
        outPatterns = outFilePatterns settings
        includeHidden = not $ excludeHidden settings

isArchiveSearchFile :: SearchSettings -> FilePath -> Bool
isArchiveSearchFile settings fp = and $ map ($fp) tests
  where tests :: [FilePath -> Bool]
        tests = [ (\x -> length inExts == 0
                         || hasInExt x)
                , (\x -> length outExts == 0
                         || not (hasOutExt x))
                , (\x -> length inPatterns == 0
                         || any (\p -> x =~ p :: Bool) inPatterns)
                , (\x -> length outPatterns == 0
                         || all (\p -> not $ x =~ p :: Bool) outPatterns)
                , (\x -> not (isHiddenFilePath x) || includeHidden)
                ]
        inExts = inArchiveExtensions settings
        hasInExt f = case getExtension f of
                       Just x -> x `elem` inExts
                       Nothing -> null inExts
        outExts = outArchiveExtensions settings
        hasOutExt f = case getExtension f of
                        Just x -> x `elem` outExts
                        Nothing -> False
        inPatterns = inArchiveFilePatterns settings
        outPatterns = outArchiveFilePatterns settings
        includeHidden = not $ excludeHidden settings

getSearchFiles :: SearchSettings -> [FilePath] -> IO [FilePath]
getSearchFiles settings dirs = do
  files <- concat `liftM` mapM getDirectoryFiles dirs
  fileTypes <- getFileTypes files
  let makeSearchFile (f,t) = SearchFile {searchFilePath=f, searchFileType=t}
  let filesWithTypes = map makeSearchFile (zip files fileTypes)
  let isSearchable sf = isSearchableFileType (searchFileType sf)
  let searchableFiles = filter isSearchable filesWithTypes
  let isArchiveFile sf = searchFileType sf == Archive
  let includeArchiveFile sf = (searchArchives settings) &&
                              isArchiveSearchFile settings (searchFilePath sf)
  let includeFile sf = (not (archivesOnly settings)) &&
                       isSearchFile settings (searchFilePath sf)
  let isValidFile sf | isArchiveFile sf = includeArchiveFile sf
                     | otherwise        = includeFile sf
  return $ map searchFilePath (filter isValidFile searchableFiles)

searchBinaryFile :: SearchSettings -> FilePath -> IO [SearchResult]
searchBinaryFile settings f = do
  blobEither <- getFileByteString f
  case blobEither of
    (Left _) -> return [] -- todo: figure out to relay error
    (Right blob) -> return $ addFilePath (searchBlob settings blob)
  where addFilePath = map (\r -> r {filePath=f})

searchBlob :: SearchSettings -> B.ByteString -> [SearchResult]
searchBlob settings blob =
  concat $ map (searchBlobForPattern settings blob) (searchPatterns settings)

searchBlobForPattern :: SearchSettings -> B.ByteString -> String -> [SearchResult]
searchBlobForPattern settings blob pattern = if hasMatch
                                             then [blobResult pattern]
                                             else []
  where hasMatch = blob =~ pattern :: Bool
        blobResult p = blankSearchResult { searchPattern=p
                                         , lineNum=0
                                         , matchStartIndex=0
                                         , matchEndIndex=0
                                         , line=B.empty
                                         }

searchTextFile :: SearchSettings -> FilePath -> IO [SearchResult]
searchTextFile settings f = do
  if (multiLineSearch settings)
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
anyMatchesAnyPattern ls ps = or (map (anyMatchesPattern ls) ps) 

anyMatchesPattern :: [B.ByteString] -> String -> Bool
anyMatchesPattern ls p = any matchesPattern ls
  where matchesPattern l = l =~ p :: Bool

searchTextFileContents :: SearchSettings -> FilePath -> IO [SearchResult]
searchTextFileContents settings f = do
  contentsEither <- getFileByteString f
  case contentsEither of
    (Left _) -> return [] -- todo: figure out to relay error
    (Right contents) -> return $ addFilePath (searchContents settings contents)
  where addFilePath = map (\r -> r {filePath=f})

searchContents :: SearchSettings -> B.ByteString -> [SearchResult]
searchContents settings contents =
  concat $ map (searchContentsForPattern settings contents) (searchPatterns settings)

searchContentsForPattern :: SearchSettings -> B.ByteString -> String -> [SearchResult]
searchContentsForPattern settings contents pattern = patternResults pattern
  where patternResults p = catMaybes (maybePatternResults p)
        maybePatternResults p = map (maybeResultFromPatternMatchIndices p) (firstOrAllIndices p)
        firstOrAllIndices p = if firstMatch settings
                              then take 1 (matchIndices contents p)
                              else matchIndices contents p
        newlineIndices =  BC.findIndices (=='\n') contents
        startLineIndices = [0] ++ map (+1) newlineIndices
        startLineIndex idx = case takeWhile (<=idx) startLineIndices of
                             [] -> 0
                             x  -> last x
        endLineIndex idx   = case dropWhile (<=idx) startLineIndices of
                             [] -> BC.length contents - 1
                             x  -> head x - 1
        lineLength i = (endLineIndex i) - (startLineIndex i)
        lineAtIndex i = B.take (lineLength i) $ B.drop (startLineIndex i) contents
        countNewlines s = BC.length $ BC.filter (=='\n') s
        linesBeforeNum = linesBefore settings
        takeRight n = reverse . take n . reverse
        linesBeforeIndices i n = (takeRight n . takeWhile (<i)) startLineIndices
        getLinesBefore i n = map lineAtIndex (linesBeforeIndices i n)
        beforeLns i | linesBeforeNum == 0 = []
                    | otherwise = getLinesBefore (startLineIndex i) linesBeforeNum
        linesAfterNum = linesAfter settings
        linesAfterIndices i n = (take n . dropWhile (<=i)) startLineIndices
        getLinesAfter i n = map lineAtIndex (linesAfterIndices i n)
        afterLns i | linesAfterNum == 0 = []
                   | otherwise = getLinesAfter (startLineIndex i) linesAfterNum
        checkBefore = (linesBefore settings) > 0
        beforeLinesMatch bs =
          not checkBefore
          || linesMatch bs (inLinesBeforePatterns settings) (outLinesBeforePatterns settings)
        checkAfter = (linesAfter settings) > 0
        afterLinesMatch as =
          not checkAfter
          || linesMatch as (inLinesAfterPatterns settings) (outLinesAfterPatterns settings)
        maybeResultFromPatternMatchIndices :: String -> (Int, Int) -> Maybe SearchResult
        maybeResultFromPatternMatchIndices p ix =
          if (beforeLinesMatch bs) && (afterLinesMatch as)
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
          where lineCount = (countNewlines (B.take (fst ix) contents)) + 1
                sli = startLineIndex (fst ix)
                msi = (fst ix) - sli
                mei = (snd ix) - sli
                bs = beforeLns (fst ix)
                as = afterLns (fst ix)

searchTextFileLines :: SearchSettings -> FilePath -> IO [SearchResult]
searchTextFileLines settings f = do
  fileLinesEither <- getFileLines f
  case fileLinesEither of
    (Left _) -> return [] -- todo: figure out to relay error
    (Right fileLines) -> return $ addFilePath (searchLineList settings fileLines)
  where addFilePath = map (\r -> r {filePath=f})

searchLineList :: SearchSettings -> [B.ByteString] -> [SearchResult]
searchLineList settings lineList = recSearchLineList [] lineList 0 []
  where recSearchLineList :: [B.ByteString] -> [B.ByteString] -> Int -> [SearchResult] -> [SearchResult]
        recSearchLineList beforeList lst num results =
          case lst of
            []     -> results
            (l:ls) -> recSearchLineList (newBefore l) ls (num + 1) (updatedResults l)
          where beforeNum = linesBefore settings
                newBefore l | beforeNum == 0 = []
                            | length beforeList == beforeNum = tail beforeList ++ [l]
                            | otherwise = beforeList ++ [l]
                afterNum = linesAfter settings
                afterList = take afterNum (tail lst)
                updatedResults l = results ++ (newResults l)
                newResults l = concat $ map (searchNextPattern l) filteredPatterns
                searchNextPattern l = searchLineForPattern settings (num + 1) beforeList l afterList
                filteredPatterns = if firstMatch settings
                                   then filter firstMatchNotMet patterns
                                   else patterns
                firstMatchNotMet p = not (any (\r -> searchPattern r == p) results)
                patterns = searchPatterns settings

searchLineForPattern :: SearchSettings -> Int -> [B.ByteString] -> B.ByteString -> [B.ByteString] -> String -> [SearchResult]
searchLineForPattern settings num bs l as pattern = patternResults pattern
  where checkBefore = (linesBefore settings) > 0
        beforeLinesMatch =
          not checkBefore
          || linesMatch bs (inLinesBeforePatterns settings) (outLinesBeforePatterns settings)
        checkAfter = (linesAfter settings) > 0
        afterLinesMatch =
          not checkAfter
          || linesMatch as (inLinesAfterPatterns settings) (outLinesAfterPatterns settings)
        lineMatchIndices :: String -> [(Int,Int)]
        lineMatchIndices p = if beforeLinesMatch && afterLinesMatch
                             then matchIndices l p
                             else []
        patternResults :: String -> [SearchResult]
        patternResults p = map (resultFromPatternMatchIndices p) (lineMatchIndices p)
        resultFromPatternMatchIndices :: String -> (Int, Int) -> SearchResult
        resultFromPatternMatchIndices p ix =
          blankSearchResult { searchPattern=p
                            , lineNum=num
                            , matchStartIndex=fst ix
                            , matchEndIndex=snd ix
                            , line=l
                            , beforeLines=bs
                            , afterLines=as
                            }

doSearchFile :: SearchSettings -> (FilePath,FileType) -> IO [SearchResult]
doSearchFile settings ft = do
  case snd ft of
    Text -> searchTextFile settings $ fst ft
    Binary -> searchBinaryFile settings $ fst ft
    _ -> return []

doSearchFiles :: SearchSettings -> [FilePath] -> IO [SearchResult]
doSearchFiles settings searchFiles = do
  fileTypes <- getFileTypes searchFiles
  results <- mapM (doSearchFile settings) (zip searchFiles fileTypes)
  return $ concat results

doSearch :: SearchSettings -> IO [SearchResult]
doSearch settings = do
  searchDirs <- getSearchDirs settings
  searchFiles <- getSearchFiles settings searchDirs
  results <- doSearchFiles settings searchFiles
  return results
