module HsSearch.SearchSettings
  ( SearchSettings(..)
  , defaultSearchSettings
  , newExtensions
  ) where

import Data.List.Split (splitOn)

import HsSearch.FileTypes
import HsSearch.FileUtil (normalizeExtension)

data SearchSettings = SearchSettings {
                                       archivesOnly :: Bool
                                     , colorize :: Bool
                                     , debug :: Bool
                                     , excludeHidden :: Bool
                                     , firstMatch :: Bool
                                     , inArchiveExtensions :: [String]
                                     , inArchiveFilePatterns :: [String]
                                     , inDirPatterns :: [String]
                                     , inExtensions :: [String]
                                     , inFilePatterns :: [String]
                                     , inFileTypes :: [FileType]
                                     , inLinesAfterPatterns :: [String]
                                     , inLinesBeforePatterns :: [String]
                                     , linesAfter :: Int
                                     , linesAfterToPatterns :: [String]
                                     , linesAfterUntilPatterns :: [String]
                                     , linesBefore :: Int
                                     , listDirs :: Bool
                                     , listFiles :: Bool
                                     , listLines :: Bool
                                     , maxLineLength :: Int
                                     , multiLineSearch :: Bool
                                     , outArchiveExtensions :: [String]
                                     , outArchiveFilePatterns :: [String]
                                     , outDirPatterns :: [String]
                                     , outExtensions :: [String]
                                     , outFilePatterns :: [String]
                                     , outFileTypes :: [FileType]
                                     , outLinesAfterPatterns :: [String]
                                     , outLinesBeforePatterns :: [String]
                                     , printResults :: Bool
                                     , printUsage :: Bool
                                     , printVersion :: Bool
                                     , recursive :: Bool
                                     , searchArchives :: Bool
                                     , searchPatterns :: [String]
                                     , startPath :: String
                                     , textFileEncoding :: String
                                     , uniqueLines :: Bool
                                     , verbose :: Bool
                                     } deriving (Show, Eq)

defaultSearchSettings :: SearchSettings
defaultSearchSettings = SearchSettings {
                                         archivesOnly=False
                                       , colorize=True
                                       , debug=False
                                       , excludeHidden=True
                                       , firstMatch=False
                                       , inArchiveExtensions=[]
                                       , inArchiveFilePatterns=[]
                                       , inDirPatterns=[]
                                       , inExtensions=[]
                                       , inFilePatterns=[]
                                       , inFileTypes=[]
                                       , inLinesAfterPatterns=[]
                                       , inLinesBeforePatterns=[]
                                       , linesAfter=0
                                       , linesAfterToPatterns=[]
                                       , linesAfterUntilPatterns=[]
                                       , linesBefore=0
                                       , listDirs=False
                                       , listFiles=False
                                       , listLines=False
                                       , maxLineLength=200
                                       , multiLineSearch=False
                                       , outArchiveExtensions=[]
                                       , outArchiveFilePatterns=[]
                                       , outDirPatterns=[]
                                       , outExtensions=[]
                                       , outFilePatterns=[]
                                       , outFileTypes=[]
                                       , outLinesAfterPatterns=[]
                                       , outLinesBeforePatterns=[]
                                       , printResults=True
                                       , printUsage=False
                                       , printVersion=False
                                       , recursive=True
                                       , searchArchives=False
                                       , searchPatterns=[]
                                       , startPath=""
                                       , textFileEncoding="utf-8"
                                       , uniqueLines=False
                                       , verbose=False
                                       }


newExtensions :: String -> [String]
newExtensions x | ',' `elem` x = map normalizeExtension $ removeBlank (splitOn "," x)
                | otherwise    = [normalizeExtension x]
  where removeBlank :: [String] -> [String]
        removeBlank = filter (/="")
