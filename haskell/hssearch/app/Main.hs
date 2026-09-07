module Main (main) where

import System.Environment (getArgs)
import System.IO (hPutStr, stderr)

import HsFind.ConsoleColor (boldRed, consoleReset)

import HsSearch.SearchConfig
import HsSearch.SearchOptions
import HsSearch.Searcher (doSearch, formatSearchResultMatchingDirs, formatSearchResultMatchingFiles,
                          formatSearchResultMatchingLines, formatSearchResultMatches, formatSearchResults,
                          getSearcher, ioValidateSearchSettings, validateSearchSettings)
import HsSearch.SearchSettings


logMsg :: String -> IO ()
logMsg = putStr

logErr :: String -> IO ()
logErr s = logErrColor s True

logErrColor :: String -> Bool -> IO ()
logErrColor s colorize =
  if colorize
    then hPutStr stderr $ boldRed ++ "ERROR: " ++ s ++ consoleReset ++ "\n"
    else hPutStr stderr $ "ERROR: " ++ s ++ "\n"

main :: IO ()
main = do
  args <- getArgs
  config' <- getSearchConfig
  searchOptionsEither <- getSearchOptions config'
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
          logMsg $ "\n" ++ getUsage searchOptions ++ "\n"
        Right settings -> do
          logMsg $ if debug settings
                   then searchSettingsToString settings ++ "\n"
                   else ""
          maybeErrMsg <- ioValidateSearchSettings settings
          case maybeErrMsg of
            Just errMsg -> do
              logMsg "\n"
              logErrColor errMsg $ colorize settings
              logMsg $ "\n" ++ getUsage searchOptions ++ "\n"
            Nothing -> do
              if printUsage settings
              then logMsg $ "\n" ++ getUsage searchOptions ++ "\n"
              else do
                searchResultsEither <- doSearch $ getSearcher config' settings
                case searchResultsEither of
                  Left errMsg -> do
                    logMsg "\n"
                    logErrColor (errMsg ++ "\n") $ colorize settings
                    logMsg $ "\n" ++ getUsage searchOptions ++ "\n"
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
