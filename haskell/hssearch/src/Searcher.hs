module Searcher
    (
      doSearch
    , getSearchDirs
    , getSearchFiles
    ) where

import qualified Data.ByteString as BL
import qualified Data.ByteString.Internal as BL (c2w)
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
  files <- mapM getDirectoryFiles dirs
  return $ filter (isSearchFile settings) $ concat files

searchTextFile :: SearchSettings -> FilePath -> IO [SearchResult]
searchTextFile settings f = do
  if (multiLineSearch settings)
    then searchTextFileContents settings f
    else searchTextFileLines settings f

matchOffsetsAndLengths :: BL.ByteString -> String -> [(MatchOffset,MatchLength)]
matchOffsetsAndLengths s p = getAllMatches $ s =~ p :: [(MatchOffset,MatchLength)]

matchIndices :: BL.ByteString -> String -> [(Int,Int)]
matchIndices s p = map (\(x,y) -> (x, x+y)) (matchOffsetsAndLengths s p)

searchTextFileContents :: SearchSettings -> FilePath -> IO [SearchResult]
searchTextFileContents settings f = do
  contentsEither <- getFileByteString f
  case contentsEither of
    (Left _) -> return [] -- todo: figure out to relay error
    (Right contents) -> return $ addFilePath (searchContents settings contents)
  where addFilePath = map (\r -> r {filePath=f})

searchContents :: SearchSettings -> BL.ByteString -> [SearchResult]
searchContents settings contents =
  concat $ map (searchContentsForPattern settings contents) (searchPatterns settings)

searchContentsForPattern :: SearchSettings -> BL.ByteString -> String -> [SearchResult]
searchContentsForPattern settings contents pattern = patternResults pattern
  where patternResults :: String -> [SearchResult]
        patternResults p = map (resultFromLinePatternIndices p) (matchIndices contents p)
        startLineIndex idx = case idx of
                             0 -> 0
                             i | BL.index contents i == (BL.c2w '\n') -> i + 1
                             i -> startLineIndex (i-1)
        endLineIndex idx = case idx of
                           i | BL.index contents i == (BL.c2w '\n') -> i
                           i | i == (BL.length contents - 1) -> i
                           i -> endLineIndex (i+1)
        lineLength i = (endLineIndex i) - (startLineIndex i)
        lineFromMatchIndex i = BL.take (lineLength i) $ BL.drop (startLineIndex i) contents
        countNewlines s = BL.length $ BL.filter (==(BL.c2w '\n')) s
        resultFromLinePatternIndices :: String -> (Int, Int) -> SearchResult
        resultFromLinePatternIndices p ix =
          blankSearchResult {searchPattern=p
                            , lineNum=(countNewlines (BL.take (fst ix) contents))+1
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

searchLineList :: SearchSettings -> [BL.ByteString] -> [SearchResult]
searchLineList settings bs = recSearchLineList bs 0 []
  where recSearchLineList :: [BL.ByteString] -> Int -> [SearchResult] -> [SearchResult]
        recSearchLineList lst num results =
          case lst of
            []     -> results
            (l:ls) -> recSearchLineList ls (num + 1) (results ++ (searchLine settings (num + 1) l))

searchLine :: SearchSettings -> Int -> BL.ByteString -> [SearchResult]
searchLine settings num l =
  concat $ map (searchLineForPattern settings num l) (searchPatterns settings)

searchLineForPattern :: SearchSettings -> Int -> BL.ByteString -> String -> [SearchResult]
searchLineForPattern settings num l pattern = patternResults pattern
  where patternResults :: String -> [SearchResult]
        patternResults p = map (resultFromLinePatternIndices p) (matchIndices l p)
        resultFromLinePatternIndices :: String -> (Int, Int) -> SearchResult
        resultFromLinePatternIndices p ix =
          blankSearchResult {searchPattern=p
                            , lineNum=num
                            , matchStartIndex=fst ix
                            , matchEndIndex=snd ix
                            , line=l
                            }

doSearchFile :: SearchSettings -> (FilePath,FileType) -> IO [SearchResult]
doSearchFile settings ft = do
  case snd ft of
    Text -> searchTextFile settings $ fst ft
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
