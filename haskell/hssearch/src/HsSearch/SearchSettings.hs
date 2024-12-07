module HsSearch.SearchSettings
  ( SearchSettings(..)
  , defaultSearchSettings
  , newExtensions
  , toFindSettings
  ) where

import Data.List.Split (splitOn)
import Data.Time (UTCTime)

import HsFind.FileTypes (FileType)
import HsFind.FileUtil (normalizeExtension)
import HsFind.FindSettings (SortBy(..))
import qualified HsFind.FindSettings as FS (FindSettings(..))

data SearchSettings = SearchSettings {
                                       archivesOnly :: Bool
                                     , colorize :: Bool
                                     , debug :: Bool
                                     , firstMatch :: Bool
                                     , followSymlinks :: Bool
                                     , inArchiveExtensions :: [String]
                                     , inArchiveFilePatterns :: [String]
                                     , includeHidden :: Bool
                                     , inDirPatterns :: [String]
                                     , inExtensions :: [String]
                                     , inFilePatterns :: [String]
                                     , inFileTypes :: [FileType]
                                     , inLinesAfterPatterns :: [String]
                                     , inLinesBeforePatterns :: [String]
                                     , linesAfter :: Integer
                                     , linesAfterToPatterns :: [String]
                                     , linesAfterUntilPatterns :: [String]
                                     , linesBefore :: Integer
                                     , maxDepth :: Integer
                                     , maxLastMod :: Maybe UTCTime
                                     , maxLineLength :: Integer
                                     , maxSize :: Integer
                                     , minDepth :: Integer
                                     , minLastMod :: Maybe UTCTime
                                     , minSize :: Integer
                                     , multiLineSearch :: Bool
                                     , outArchiveExtensions :: [String]
                                     , outArchiveFilePatterns :: [String]
                                     , outDirPatterns :: [String]
                                     , outExtensions :: [String]
                                     , outFilePatterns :: [String]
                                     , outFileTypes :: [FileType]
                                     , outLinesAfterPatterns :: [String]
                                     , outLinesBeforePatterns :: [String]
                                     , paths :: [String]
                                     , printDirs :: Bool
                                     , printFiles :: Bool
                                     , printLines :: Bool
                                     , printResults :: Bool
                                     , printUsage :: Bool
                                     , printVersion :: Bool
                                     , recursive :: Bool
                                     , searchArchives :: Bool
                                     , searchPatterns :: [String]
                                     , sortCaseInsensitive :: Bool
                                     , sortDescending :: Bool
                                     , sortResultsBy :: SortBy
                                     , textFileEncoding :: String
                                     , uniqueLines :: Bool
                                     , verbose :: Bool
                                     } deriving (Show, Eq)

defaultSearchSettings :: SearchSettings
defaultSearchSettings = SearchSettings {
                                         archivesOnly=False
                                       , colorize=True
                                       , debug=False
                                       , firstMatch=False
                                       , followSymlinks=False
                                       , inArchiveExtensions=[]
                                       , inArchiveFilePatterns=[]
                                       , includeHidden=False
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
                                       , maxDepth = -1
                                       , maxLineLength=200
                                       , maxLastMod=Nothing
                                       , maxSize=0
                                       , minDepth = -1
                                       , minLastMod=Nothing
                                       , minSize=0
                                       , multiLineSearch=False
                                       , outArchiveExtensions=[]
                                       , outArchiveFilePatterns=[]
                                       , outDirPatterns=[]
                                       , outExtensions=[]
                                       , outFilePatterns=[]
                                       , outFileTypes=[]
                                       , outLinesAfterPatterns=[]
                                       , outLinesBeforePatterns=[]
                                       , paths=[]
                                       , printDirs=False
                                       , printFiles=False
                                       , printLines=False
                                       , printResults=False
                                       , printUsage=False
                                       , printVersion=False
                                       , recursive=True
                                       , searchArchives=False
                                       , searchPatterns=[]
                                       , sortCaseInsensitive=False
                                       , sortDescending=False
                                       , sortResultsBy=SortByFilePath
                                       , textFileEncoding="utf-8"
                                       , uniqueLines=False
                                       , verbose=False
                                       }


newExtensions :: String -> [String]
newExtensions x | ',' `elem` x = map normalizeExtension $ removeBlank (splitOn "," x)
                | otherwise    = [normalizeExtension x]
  where removeBlank :: [String] -> [String]
        removeBlank = filter (/="")

toFindSettings :: SearchSettings -> FS.FindSettings
toFindSettings searchSettings = FS.FindSettings {
                                     FS.archivesOnly=archivesOnly searchSettings
                                   , FS.debug=debug searchSettings
                                   , FS.followSymlinks=followSymlinks searchSettings
                                   , FS.inArchiveExtensions=inArchiveExtensions searchSettings
                                   , FS.inArchiveFilePatterns=inArchiveFilePatterns searchSettings
                                   , FS.includeHidden=includeHidden searchSettings
                                   , FS.inDirPatterns=inDirPatterns searchSettings
                                   , FS.inExtensions=inExtensions searchSettings
                                   , FS.inFilePatterns=inFilePatterns searchSettings
                                   , FS.inFileTypes=inFileTypes searchSettings
                                   , FS.includeArchives=searchArchives searchSettings
                                   , FS.maxDepth=maxDepth searchSettings
                                   , FS.maxLastMod=maxLastMod searchSettings
                                   , FS.maxSize=maxSize searchSettings
                                   , FS.minDepth=minDepth searchSettings
                                   , FS.minLastMod=minLastMod searchSettings
                                   , FS.minSize=minSize searchSettings
                                   , FS.outArchiveExtensions=outArchiveExtensions searchSettings
                                   , FS.outArchiveFilePatterns=outArchiveFilePatterns searchSettings
                                   , FS.outDirPatterns=outDirPatterns searchSettings
                                   , FS.outExtensions=outExtensions searchSettings
                                   , FS.outFilePatterns=outFilePatterns searchSettings
                                   , FS.outFileTypes=outFileTypes searchSettings
                                   , FS.paths=paths searchSettings
                                   , FS.printDirs=printDirs searchSettings
                                   , FS.printFiles=printFiles searchSettings
                                   , FS.printUsage=printUsage searchSettings
                                   , FS.printVersion=printVersion searchSettings
                                   , FS.recursive=recursive searchSettings
                                   , FS.sortCaseInsensitive=sortCaseInsensitive searchSettings
                                   , FS.sortDescending=sortDescending searchSettings
                                   , FS.sortResultsBy=sortResultsBy searchSettings
                                   , FS.verbose=verbose searchSettings
                                   }
