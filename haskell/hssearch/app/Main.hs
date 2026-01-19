module Main (main) where

import System.Environment (getArgs)
import System.IO (hPutStr, stderr)

import HsFind.ConsoleColor (boldRed, consoleReset)

import HsSearch.SearchOptions
import HsSearch.Searcher (doSearch, formatSearchResultMatchingDirs, formatSearchResultMatchingFiles,
                          formatSearchResultMatchingLines, formatSearchResultMatches, formatSearchResults,
                          validateSearchSettings)
import HsSearch.SearchSettings


logMsg :: String -> IO ()
logMsg = putStr

logErr :: String -> IO ()
logErr s = hPutStr stderr $ "ERROR: " ++ s

logErrColor :: String -> Bool -> IO ()
logErrColor s colorize =
  if colorize
    then hPutStr stderr $ boldRed ++ "ERROR: " ++ s ++ consoleReset ++ "\n"
    else hPutStr stderr $ "ERROR: " ++ s ++ "\n"

main :: IO ()
main = do
  args <- getArgs
  searchOptionsEither <- getSearchOptions
  case searchOptionsEither of
    Left errMsg -> do
      logMsg "\n"
      logErr $ errMsg ++ "\n"
    Right searchOptions -> do
      settingsFromArgsEither <- settingsFromArgs searchOptions args
      case settingsFromArgsEither of
        Left errMsg -> do
          logMsg "\n"
          logErr $ errMsg ++ "\n"
          logMsg $ "\n" ++ getUsage (options searchOptions) ++ "\n"
        Right settings -> do
          logMsg $ if debug settings
                   then searchSettingsToString settings ++ "\n"
                   else ""
          case validateSearchSettings settings of
            Just errMsg -> do
              logMsg "\n"
              logErrColor errMsg $ colorize settings
              logMsg $ "\n" ++ getUsage (options searchOptions) ++ "\n"
            Nothing -> do
              if printUsage settings
              then logMsg $ "\n" ++ getUsage (options searchOptions) ++ "\n"
              else do
                searchResultsEither <- doSearch settings
                case searchResultsEither of
                  Left errMsg -> do
                    logMsg "\n"
                    logErr $ errMsg ++ "\n"
                    logMsg $ "\n" ++ getUsage (options searchOptions) ++ "\n"
                  Right searchResults -> do
                    logMsg $ formatSearchResults settings searchResults
                    logMsg $ if printDirs settings
                             then formatSearchResultMatchingDirs settings searchResults
                             else ""
                    logMsg $ if printFiles settings
                             then formatSearchResultMatchingFiles settings searchResults
                             else ""
                    logMsg $ if printLines settings
                             then formatSearchResultMatchingLines settings searchResults
                             else ""
                    logMsg $ if printMatches settings
                             then formatSearchResultMatches settings searchResults
                             else ""
                    logMsg ""
