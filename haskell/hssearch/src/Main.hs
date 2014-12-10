module Main where

import Data.List (nubBy, sort)
import System.Environment (getArgs)
import System.FilePath (takeDirectory)

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
        
formatResults :: SearchSettings -> [SearchResult] -> String
formatResults settings results =
  "\nSearch results (" ++ show (length results) ++ "):" ++
    (if length results > 0
       then "\n" ++ unlines (map formatSearchResult results)
       else "") ++
    (concat (map matchString (searchPatterns settings)))
  where matchString p = "\nMatches for "++ show p ++ ": " ++ show (patternCount p)
        patternCount :: String -> Int
        patternCount p = foldr (\x acc -> if p == (searchPattern x) then acc + 1 else acc) 0 results

getMatchingDirs :: [SearchResult] -> [FilePath]
getMatchingDirs results = sort $ map (\r -> takeDirectory (filePath r)) (nubBy matchingDirs results)
  where matchingDirs x y = takeDirectory (filePath x) == takeDirectory (filePath y)

formatMatchingDirs :: [SearchResult] -> String
formatMatchingDirs results = 
  "\n\nDirectories with matches (" ++ show (length matchingDirs) ++ "):\n" ++
  unlines matchingDirs
  where matchingDirs = getMatchingDirs results

getMatchingFiles :: [SearchResult] -> [FilePath]
getMatchingFiles results = sort $ map (\r -> filePath r) (nubBy matchingFilePaths results)
  where matchingFilePaths x y = filePath x == filePath y

formatMatchingFiles :: [SearchResult] -> String
formatMatchingFiles results = 
  "\n\nFiles with matches (" ++ show (length matchingFiles) ++ "):\n" ++
  unlines matchingFiles
  where matchingFiles = getMatchingFiles results

main :: IO ()
main = do
  args <- getArgs
  searchOptions <- getSearchOptions
  let settings = settingsFromArgs searchOptions args
  putStr $ if debug settings
           then "\nsettingsFromArgs " ++ show args ++ ": " ++ show settings ++
                "\n"
           else ""
  let maybeUsage = errsOrUsage searchOptions settings
  case maybeUsage of
    Just usage -> putStrLn usage
    Nothing -> do
      searchDirs <- getSearchDirs settings
      putStr $ if verbose settings
               then "\nDirectories to be searched (" ++
                    show (length searchDirs) ++ "):\n" ++
                    unlines searchDirs
               else ""
      searchFiles <- getSearchFiles settings searchDirs
      putStr $ if verbose settings
               then "\nFiles to be searched (" ++
                    show (length searchFiles) ++ "):\n" ++
                    unlines searchFiles
               else ""
      results <- doSearchFiles settings searchFiles
      putStr $ if printResults settings
               then formatResults settings results
               else ""
      putStr $ if listDirs settings
               then formatMatchingDirs results
               else ""
      putStr $ if listFiles settings
               then formatMatchingFiles results
               else ""

      putStrLn ""
