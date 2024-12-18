module Main (main) where

import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BC
import Data.Char (isSpace, toUpper)
import Data.List (nub, sort, sortBy)
import System.Environment (getArgs)
import System.FilePath (takeDirectory)
import System.IO (hPutStr, stderr)

import HsSearch.SearchOptions
import HsSearch.Searcher (getSearchFiles, doSearch, doSearchFiles, validateSearchSettings)
import HsSearch.SearchResult
import HsSearch.SearchSettings


formatResults :: SearchSettings -> [SearchResult] -> String
formatResults settings results =
  "\nSearch results (" ++ show (length results) ++ "):\n" ++
    (if not (null results)
       then unlines (map (formatSearchResult settings) results)
       else "")

getMatchingDirs :: [SearchResult] -> [FilePath]
getMatchingDirs = sort . nub . map getDirectory
  where getDirectory r = takeDirectory (filePath r)

formatMatchingDirs :: [SearchResult] -> String
formatMatchingDirs results = 
  if not (null matchingDirs) then
    "\nMatching directories (" ++ show (length matchingDirs) ++ "):\n" ++
    unlines matchingDirs
  else "\nMatching directories: 0\n"
  where matchingDirs = getMatchingDirs results

getMatchingFiles :: [SearchResult] -> [FilePath]
getMatchingFiles = sort . nub . map filePath

formatMatchingFiles :: [SearchResult] -> String
formatMatchingFiles results = 
  if not (null matchingFiles) then
    "\nMatching files (" ++ show (length matchingFiles) ++ "):\n" ++
    unlines matchingFiles
  else "\nMatching files: 0\n"
  where matchingFiles = getMatchingFiles results

byteStringToUpper :: B.ByteString -> B.ByteString
byteStringToUpper = BC.pack . map toUpper . BC.unpack

doSortCaseInsensitive :: [B.ByteString] -> [B.ByteString]
doSortCaseInsensitive = sortBy compareCaseInsensitive
  where compareCaseInsensitive a b = byteStringToUpper a `compare` byteStringToUpper b

getMatchingLines :: [SearchResult] -> Bool -> [B.ByteString]
getMatchingLines results unique | unique = (doSortCaseInsensitive . nub . map trimLine) results
                                | otherwise = (doSortCaseInsensitive . map trimLine) results
  where trimLine = BC.dropWhile isSpace . line

formatMatchingLines :: [SearchResult] -> Bool -> String
formatMatchingLines results unique = 
  "\n" ++ hdrText ++ " (" ++ show (length matchingLines) ++ "):\n" ++
  BC.unpack (BC.intercalate (BC.pack "\n") matchingLines) ++ "\n"
  where matchingLines = getMatchingLines results unique
        hdrText = if unique
                  then "Unique lines with matches"
                  else "Lines with matches"

logMsg :: String -> IO ()
logMsg = putStr

logErr :: String -> IO ()
logErr s = hPutStr stderr $ "ERROR: " ++ s

main :: IO ()
main = do
  args <- getArgs
  searchOptions <- getSearchOptions
  case settingsFromArgs searchOptions args of
    Left errMsg -> do
      logMsg "\n"
      logErr $ errMsg ++ "\n"
      logMsg $ getUsage searchOptions ++ "\n"
    Right settings -> do
      logMsg $ if debug settings
               then "\nsettings: " ++ show settings ++ "\n"
               else ""
      case validateSearchSettings settings of
        Just errMsg -> do
          logMsg "\n"
          logErr $ errMsg ++ "\n"
          logMsg $ getUsage searchOptions ++ "\n"
        Nothing -> do
          if printUsage settings
          then logMsg $ getUsage searchOptions ++ "\n"
          else do
            searchResultsEither <- doSearch settings
            case searchResultsEither of
              Left errMsg -> do
                logMsg "\n"
                logErr $ errMsg ++ "\n"
                logMsg "\n"
                logMsg $ getUsage searchOptions ++ "\n"
              Right searchResults -> do
                logMsg $ formatResults settings searchResults
                logMsg $ if printDirs settings
                         then formatMatchingDirs searchResults
                         else ""
                logMsg $ if printFiles settings
                         then formatMatchingFiles searchResults
                         else ""
                logMsg $ if printLines settings
                         then formatMatchingLines searchResults (uniqueLines settings)
                         else ""
                logMsg ""
