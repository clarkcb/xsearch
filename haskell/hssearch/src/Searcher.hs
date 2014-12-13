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
import Text.Regex.PCRE

import FileTypes
import FileUtility
import SearchResult
import SearchSettings

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
                         || isInExt x)
                , (\x -> length outExts == 0
                         || not (isOutExt x))
                , (\x -> length inPatterns == 0
                         || any (\p -> x =~ p :: Bool) inPatterns)
                , (\x -> length outPatterns == 0
                         || all (\p -> not $ x =~ p :: Bool) outPatterns)
                , (\x -> not (isHiddenFilePath x) || includeHidden)
                ]
        isInExt f = case getExtension f of
                      Just x -> x `elem` inExts
                      Nothing -> null inExts
        inExts = inExtensions settings
        isOutExt f = case getExtension f of
                       Just x -> x `elem` outExts
                       Nothing -> False
        outExts = outExtensions settings
        inPatterns = inFilePatterns settings
        outPatterns = outFilePatterns settings
        includeHidden = not $ excludeHidden settings

getSearchFiles :: SearchSettings -> [FilePath] -> IO [FilePath]
getSearchFiles settings dirs = do
  files <- concat `liftM` mapM getDirectoryFiles dirs
  fileTypes <- getFileTypes files
  let filesWithTypes = zip files fileTypes
  let isSearchable ft = isSearchableFileType (snd ft)
  let searchableFiles = map (\ft -> fst ft) (filter isSearchable filesWithTypes)
  return $ filter (isSearchFile settings) searchableFiles

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
  where patternResults p = map (resultFromLinePatternIndices p) (firstOrAllIndices p)
        firstOrAllIndices p = if firstMatch settings
                              then take 1 (matchIndices contents p)
                              else matchIndices contents p
        startLineIndex idx = case idx of
                             0 -> 0
                             i | BC.index contents i == '\n' -> i + 1
                             i -> startLineIndex (i-1)
        endLineIndex idx = case idx of
                           i | BC.index contents i == '\n' -> i
                           i | i == (B.length contents - 1) -> i
                           i -> endLineIndex (i+1)
        lineLength i = (endLineIndex i) - (startLineIndex i)
        lineFromMatchIndex i = B.take (lineLength i) $ B.drop (startLineIndex i) contents
        countNewlines s = BC.length $ BC.filter (=='\n') s
        resultFromLinePatternIndices :: String -> (Int, Int) -> SearchResult
        resultFromLinePatternIndices p ix =
          blankSearchResult { searchPattern=p
                            , lineNum=(countNewlines (B.take (fst ix) contents))+1
                            , matchStartIndex=(fst ix) - (startLineIndex (fst ix))
                            , matchEndIndex=(snd ix) - (startLineIndex (fst ix))
                            , line=lineFromMatchIndex (fst ix)
                            }

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
  where patternResults :: String -> [SearchResult]
        patternResults p = map (resultFromLinePatternIndices p) (matchIndices l p)
        resultFromLinePatternIndices :: String -> (Int, Int) -> SearchResult
        resultFromLinePatternIndices p ix =
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
