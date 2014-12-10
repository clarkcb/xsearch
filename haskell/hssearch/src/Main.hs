module Main where

import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BC
import Data.List (nub, sort)
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
        patternCount p = foldr accMatches 0 results
          where accMatches x acc = if p == (searchPattern x) then acc + 1 else acc

getMatchingDirs :: [SearchResult] -> [FilePath]
getMatchingDirs results = (sort . nub . map getDirectory) results
  where getDirectory r = takeDirectory (filePath r)

formatMatchingDirs :: [SearchResult] -> String
formatMatchingDirs results = 
  "\n\nDirectories with matches (" ++ show (length matchingDirs) ++ "):\n" ++
  unlines matchingDirs
  where matchingDirs = getMatchingDirs results

getMatchingFiles :: [SearchResult] -> [FilePath]
getMatchingFiles results = (sort . nub . map filePath) results

formatMatchingFiles :: [SearchResult] -> String
formatMatchingFiles results = 
  "\n\nFiles with matches (" ++ show (length matchingFiles) ++ "):\n" ++
  unlines matchingFiles
  where matchingFiles = getMatchingFiles results

getMatchingLines :: [SearchResult] -> [B.ByteString]
getMatchingLines results = (sort . nub . map trimLine) results
  where trimLine = BC.dropWhile isWhitespace . line
        isWhitespace c = c `elem` [' ', '\t']

formatMatchingLines :: [SearchResult] -> String
formatMatchingLines results = 
  "\n\nLines with matches (" ++ show (length matchingLines) ++ "):\n" ++
  BC.unpack (BC.intercalate (BC.pack "\n") matchingLines)
  where matchingLines = getMatchingLines results

formatSearchDirs :: [FilePath] -> String
formatSearchDirs dirs = 
  "\nDirectories to be searched (" ++ show (length dirs) ++ "):\n" ++
  unlines dirs

formatSearchFiles :: [FilePath] -> String
formatSearchFiles files = 
  "\nFiles to be searched (" ++ show (length files) ++ "):\n" ++
  unlines files

main :: IO ()
main = do
  args <- getArgs
  searchOptions <- getSearchOptions
  let settings = settingsFromArgs searchOptions args
  putStr $ if debug settings
           then "\nsettingsFromArgs " ++ show args ++ ": " ++ show settings ++
                "\n"
           else ""
  case errsOrUsage searchOptions settings of
    Just usage -> putStrLn usage
    Nothing -> do
      searchDirs <- getSearchDirs settings
      putStr $ if verbose settings
               then formatSearchDirs searchDirs
               else ""
      searchFiles <- getSearchFiles settings searchDirs
      putStr $ if verbose settings
               then formatSearchFiles searchFiles
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
      putStr $ if listLines settings
               then formatMatchingLines results
               else ""
      putStrLn ""
