module Main where

import System.Environment (getArgs)

import SearchOptions
import Searcher
import SearchResult
import SearchSettings


validateSettings :: SearchSettings -> [String]
validateSettings settings = concat $ map ($settings) validators
  where validators = [ (\s -> if startPath s == ""
                              then ["Startpath not defined"]
                              else [])
                     , (\s -> if length (searchPatterns s) == 0
                              then ["No search patterns defined"]
                              else [])
                     ]

errsOrUsage :: [SearchOption] -> SearchSettings -> Maybe String
errsOrUsage searchOptions settings = do
  case usage of
    "" -> Nothing
    _  -> Just usage
  where errs   = validateSettings settings
        errMsg = if length errs > 0
                 then "\nERROR:\n" ++ unlines errs ++ "\n"
                 else ""
        usage  = if length errMsg > 0 || printUsage settings
                 then errMsg ++ getUsage searchOptions
                 else ""
        
formatResults :: [SearchResult] -> String
formatResults results = if length results > 0
  then "\nSearch results (" ++ show (length results) ++ "):\n" ++ unlines (map formatSearchResult results)
  else "\nSearch results: none"

main :: IO ()
main = do
  args <- getArgs

  searchOptions <- getSearchOptions
  let settings = settingsFromArgs searchOptions args
  putStrLn $ if debug settings
             then "\nsettingsFromArgs " ++ show args ++ ": " ++ show settings
             else ""

  let maybeUsage = errsOrUsage searchOptions settings
  case maybeUsage of
    Just usage -> putStrLn usage
    Nothing -> do
      putStrLn $ "Performing search... "
      searchDirs <- getSearchDirs settings
      putStrLn $ "searchDirs (" ++ show (length searchDirs) ++ "):\n" ++ unlines searchDirs
      searchFiles <- getSearchFiles settings searchDirs
      putStrLn $ "searchFiles (" ++ show (length searchFiles) ++ "):\n" ++ unlines searchFiles
      --searchFileTypes <- getFileTypes searchFiles
      --putStrLn $ "searchFileTypes (" ++ show (length searchFileTypes) ++ "):\n" ++ unlines (map show searchFileTypes)
      results <- doSearch settings
      putStrLn $ formatResults results
