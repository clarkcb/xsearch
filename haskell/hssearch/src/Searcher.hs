module Searcher
    (
      doSearch
    , getSearchDirs
    , getSearchFiles
    ) where


import qualified Data.ByteString as BL
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

searchTextFileContents :: SearchSettings -> FilePath -> IO [SearchResult]
searchTextFileContents settings f = do
  return []

searchTextFileLines :: SearchSettings -> FilePath -> IO [SearchResult]
searchTextFileLines settings f = do
  fileLinesEither <- getFileLines f
  case fileLinesEither of
    (Left e) -> return [] -- todo: figure out to relay error
    (Right fileLines) -> return $ setFilePath (searchLineList settings fileLines)
  where setFilePath = map (\r -> r {filePath=f})

searchLineList :: SearchSettings -> [BL.ByteString] -> [SearchResult]
searchLineList settings ls = recSearchLineList ls 0 []
  where recSearchLineList :: [BL.ByteString] -> Int -> [SearchResult] -> [SearchResult]
        recSearchLineList lst lineNum results =
          case lst of
            []     -> results
            (l:ls) -> recSearchLineList ls (lineNum + 1) (results ++ (searchLine settings (lineNum + 1) l))

searchLine :: SearchSettings -> Int -> BL.ByteString -> [SearchResult]
searchLine settings lineNum line = concat $ map (searchLineForPattern settings lineNum line) (searchPatterns settings)

searchLineForPattern :: SearchSettings -> Int -> BL.ByteString -> String -> [SearchResult]
searchLineForPattern settings lineNum line pattern = patternResults lineNum line pattern
  where patternResults :: Int -> BL.ByteString -> String -> [SearchResult]
        patternResults n l p = map (resultFromLinePatternIndices n l p) (matchIndices l p)
        matchIndices :: BL.ByteString -> String -> [(MatchOffset,MatchLength)]
        matchIndices l p = getAllMatches $ l =~ p :: [(MatchOffset,MatchLength)]
        resultFromLinePatternIndices :: Int -> BL.ByteString -> String -> (MatchOffset, MatchLength) -> SearchResult
        resultFromLinePatternIndices n l p ix =
          blankSearchResult {searchPattern=p
                            , lineNum=n
                            , matchStartIndex=(fst ix)
                            , matchEndIndex=((fst ix) + (snd ix))
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
