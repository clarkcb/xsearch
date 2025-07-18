module Main (main) where

import System.Environment (getArgs)
import System.IO (hPutStr, stderr)

import HsSearch.SearchOptions
import HsSearch.Searcher (doSearch, formatMatchingDirs, formatMatchingFiles, formatMatchingLines,
                          formatResults, validateSearchSettings)
import HsSearch.SearchSettings


logMsg :: String -> IO ()
logMsg = putStr

logErr :: String -> IO ()
logErr s = hPutStr stderr $ "ERROR: " ++ s

main :: IO ()
main = do
  args <- getArgs
  searchOptionsEither <- getSearchOptions
  case searchOptionsEither of
    Left errMsg -> do
      logMsg "\n"
      logErr $ errMsg ++ "\n"
    Right searchOptions -> do
      settingsFromArgsEither <- ioSettingsFromArgs searchOptions args
      case settingsFromArgsEither of
        Left errMsg -> do
          logMsg "\n"
          logErr $ errMsg ++ "\n"
          logMsg $ "\n" ++ getUsage searchOptions ++ "\n"
        Right settings -> do
          logMsg $ if debug settings
                   then searchSettingsToString settings ++ "\n"
                   else ""
          case validateSearchSettings settings of
            Just errMsg -> do
              logMsg "\n"
              logErr $ errMsg ++ "\n"
              logMsg $ "\n" ++ getUsage searchOptions ++ "\n"
            Nothing -> do
              if printUsage settings
              then logMsg $ "\n" ++ getUsage searchOptions ++ "\n"
              else do
                searchResultsEither <- doSearch settings
                case searchResultsEither of
                  Left errMsg -> do
                    logMsg "\n"
                    logErr $ errMsg ++ "\n"
                    logMsg $ "\n" ++ getUsage searchOptions ++ "\n"
                  Right searchResults -> do
                    logMsg $ formatResults settings searchResults
                    logMsg $ if printDirs settings
                             then formatMatchingDirs settings searchResults
                             else ""
                    logMsg $ if printFiles settings
                             then formatMatchingFiles settings searchResults
                             else ""
                    logMsg $ if printLines settings
                             then formatMatchingLines settings searchResults
                             else ""
                    logMsg ""
