{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
module HsSearch.SearchSettings
  ( SearchSettings(..)
  , defaultSearchSettings
  , newExtensions
  , searchSettingsToString
  , toFindSettings
  , updateSearchSettingsFromJsonValue
  ) where

import Control.Monad (mzero, unless, MonadFail (fail))
import Data.Aeson
import Data.Aeson.Types (Parser)
import Data.List (intercalate, sort)
import Data.List.Split (splitOn)
import Data.Maybe (isJust)
import Data.Text (Text, unpack)
import Data.Time (UTCTime, parseTimeM, defaultTimeLocale)
import qualified Data.HashMap.Strict as HM
import qualified Data.Vector as V (toList)
import GHC.Generics (Generic)

import HsFind.FileTypes (FileType, getFileTypeForName, getFileTypeName)
import HsFind.FileUtil (normalizeExtension)
import qualified HsFind.FindSettings as FS (FindSettings(..))
import HsFind.SortBy (SortBy(..), getSortByForName, sortByToString)

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

searchSettingsToString :: SearchSettings -> String
searchSettingsToString settings = 
  "SearchSettings(" ++
  "archivesOnly=" ++ show (archivesOnly settings) ++
  ", colorize=" ++ show (colorize settings) ++
  ", debug=" ++ show (debug settings) ++
  ", firstMatch=" ++ show (firstMatch settings) ++
  ", followSymlinks=" ++ show (followSymlinks settings) ++
  ", inArchiveExtensions=" ++ listToString (inArchiveExtensions settings) ++
  ", inArchiveFilePatterns=" ++ listToString (inArchiveFilePatterns settings) ++
  ", includeHidden=" ++ show (includeHidden settings) ++
  ", inDirPatterns=" ++ listToString (inDirPatterns settings) ++
  ", inExtensions=" ++ listToString (inExtensions settings) ++
  ", inFilePatterns=" ++ listToString (inFilePatterns settings) ++
  ", inFileTypes=" ++ fileTypesToString (inFileTypes settings) ++
  ", inLinesAfterPatterns=" ++ listToString (inLinesAfterPatterns settings) ++
  ", inLinesBeforePatterns=" ++ listToString (inLinesBeforePatterns settings) ++
  ", linesAfter=" ++ show (linesAfter settings) ++
  ", linesAfterToPatterns=" ++ listToString (linesAfterToPatterns settings) ++
  ", linesAfterUntilPatterns=" ++ listToString (linesAfterUntilPatterns settings) ++
  ", linesBefore=" ++ show (linesBefore settings) ++
  ", maxDepth=" ++ show (maxDepth settings) ++
  ", maxLastMod=" ++ lastModToString (maxLastMod settings) ++
  ", maxLineLength=" ++ show (maxLineLength settings) ++
  ", maxSize=" ++ show (maxSize settings) ++
  ", minDepth=" ++ show (minDepth settings) ++
  ", minLastMod=" ++ lastModToString (minLastMod settings) ++
  ", minSize=" ++ show (minSize settings) ++
  ", multiLineSearch=" ++ show (multiLineSearch settings) ++
  ", outArchiveExtensions=" ++ listToString (outArchiveExtensions settings) ++
  ", outArchiveFilePatterns=" ++ listToString (outArchiveFilePatterns settings) ++
  ", outDirPatterns=" ++ listToString (outDirPatterns settings) ++
  ", outExtensions=" ++ listToString (outExtensions settings) ++
  ", outFilePatterns=" ++ listToString (outFilePatterns settings) ++
  ", outFileTypes=" ++ fileTypesToString (outFileTypes settings) ++
  ", outLinesAfterPatterns=" ++ listToString (outLinesAfterPatterns settings) ++
  ", outLinesBeforePatterns=" ++ listToString (outLinesBeforePatterns settings) ++
  ", paths=" ++ listToString (paths settings) ++
  ", printDirs=" ++ show (printDirs settings) ++
  ", printFiles=" ++ show (printFiles settings) ++
  ", printLines=" ++ show (printLines settings) ++
  ", printUsage=" ++ show (printUsage settings) ++
  ", printResults=" ++ show (printResults settings) ++
  ", printVersion=" ++ show (printVersion settings) ++
  ", recursive=" ++ show (recursive settings) ++
  ", searchArchives=" ++ show (searchArchives settings) ++
  ", searchPatterns=" ++ listToString (searchPatterns settings) ++
  ", sortBy=" ++ sortByToString (sortResultsBy settings) ++
  ", sortCaseInsensitive=" ++ show (sortCaseInsensitive settings) ++
  ", sortDescending=" ++ show (sortDescending settings) ++
  ", textFileEncoding=\"" ++ textFileEncoding settings ++ "\"" ++
  ", uniqueLines=" ++ show (uniqueLines settings) ++
  ", verbose=" ++ show (verbose settings) ++
  ")"
  where listToString lst | null lst = "[]"
                         | otherwise = "[\"" ++ intercalate "\", \"" lst ++ "\"]"
        fileTypesToString fts = "[" ++ intercalate ", " (fileTypeNames fts) ++ "]"
        fileTypeNames = Prelude.map getFileTypeName
        lastModToString :: Maybe UTCTime -> String
        lastModToString Nothing = "0"
        lastModToString (Just t) = show t

-- JSON parsing stuff below here
validKeys :: [Text]
validKeys = ["allmatches", "archivesonly", "colorize", "debug", "encoding", "excludehidden",
             "firstmatch", "followsymlinks", "help", "includehidden", "in-archiveext",
             "in-archivefilepattern", "in-dirpattern", "in-ext", "in-filepattern",
             "in-filetype", "in-linesafterpattern", "in-linesbeforepattern", "linesafter",
             "linesaftertopattern", "linesafteruntilpattern", "linesbefore", "maxdepth",
             "maxlastmod", "maxlinelength", "maxsize", "mindepth", "minlastmod", "minsize",
             "multilinesearch", "nocolorize", "nofollowsymlinks", "noprintdirs",
             "noprintfiles", "noprintlines", "noprintmatches", "norecursive",
             "nosearcharchives", "out-archiveext", "out-archivefilepattern", "out-dirpattern",
             "out-ext", "out-filepattern", "out-filetype", "out-linesafterpattern",
             "out-linesbeforepattern", "path", "printdirs", "printfiles", "printlines",
             "printmatches", "recursive", "searchpattern", "searcharchives", "settings-file",
             "sort-by", "sort-ascending", "sort-casesensitive", "sort-caseinsensitive",
             "sort-descending", "uniquelines", "verbose", "version"]

-- for now, we do not have custom JSON parsing for boolean fields with "no-" counterparts,
-- the user should just use the positive counterpart with true/false
instance FromJSON SearchSettings where
  parseJSON = withObject "SearchSettings" $ \obj -> do
    -- Check for unknown keys
    let keysInJson = HM.keys obj
        -- keys are sorted so that output is consistent across all versions
        unknownKeys = sort $ filter (`notElem` validKeys) keysInJson
    unless (null unknownKeys) $
      fail $ "Invalid option: " ++ unpack (head unknownKeys)

    -- Parse known fields
    archivesOnly <- obj .:? "archivesonly" .!= False
    colorize <- obj .:? "colorize" .!= True
    debug <- obj .:? "debug" .!= False
    firstMatch <- parseFirstMatch obj
    followSymlinks <- obj .:? "followsymlinks" .!= False
    inArchiveExtensions <- obj .:? "in-archiveext" >>= parseStringOrArray
    inArchiveFilePatterns <- obj .:? "in-archivefilepattern" >>= parseStringOrArray
    inDirPatterns <- obj .:? "in-dirpattern" >>= parseStringOrArray
    inExtensions <- obj .:? "in-ext" >>= parseStringOrArray
    inFilePatterns <- obj .:? "in-filepattern" >>= parseStringOrArray
    inFileTypes <- obj .:? "in-filetype" >>= parseFileTypes
    inLinesAfterPatterns <- obj .:? "in-linesafterpattern" >>= parseStringOrArray
    inLinesBeforePatterns <- obj .:? "in-linesbeforepattern" >>= parseStringOrArray
    includeHidden <- parseIncludeHidden obj
    linesAfter <- obj .:? "linesafter" .!= 0
    linesAfterToPatterns <- obj .:? "linesaftertopattern" >>= parseStringOrArray
    linesAfterUntilPatterns <- obj .:? "linesafteruntilpattern" >>= parseStringOrArray
    linesBefore <- obj .:? "linesbefore" .!= 0
    maxDepth <- obj .:? "maxdepth" .!= (-1)
    maxLastMod <- obj .:? "maxlastmod" >>= parseUTCTime
    maxLineLength <- obj .:? "maxlinelength" .!= 0
    maxSize <- obj .:? "maxsize" .!= 0
    minDepth <- obj .:? "mindepth" .!= (-1)
    minLastMod <- obj .:? "minlastmod" >>= parseUTCTime
    minSize <- obj .:? "minsize" .!= 0
    multiLineSearch <- obj .:? "multilinesearch" .!= False
    outArchiveExtensions <- obj .:? "out-archiveextension" >>= parseStringOrArray
    outArchiveFilePatterns <- obj .:? "out-archivefilepattern" >>= parseStringOrArray
    outDirPatterns <- obj .:? "out-dirpattern" >>= parseStringOrArray
    outExtensions <- obj .:? "out-ext" >>= parseStringOrArray
    outFilePatterns <- obj .:? "out-filepattern" >>= parseStringOrArray
    outFileTypes <- obj .:? "out-filetype" >>= parseFileTypes
    outLinesAfterPatterns <- obj .:? "out-linesafterpattern" >>= parseStringOrArray
    outLinesBeforePatterns <- obj .:? "out-linesbeforepattern" >>= parseStringOrArray
    paths <- obj .:? "path" >>= parseStringOrArray
    printDirs <- obj .:? "printdirs" .!= False
    printFiles <- obj .:? "printfiles" .!= False
    printLines <- obj .:? "printlines" .!= False
    printResults <- obj .:? "printmatches" .!= False
    printUsage <- obj .:? "help" .!= False
    printVersion <- parsePrintVersion obj
    recursive <- obj .:? "recursive" .!= True
    searchArchives <- obj .:? "searcharchives" .!= True
    searchPatterns <- obj .:? "searchpattern" >>= parseStringOrArray
    sortCaseInsensitive <- obj .:? "sort-caseinsensitive" .!= False
    sortDescending <- obj .:? "sort-descending" .!= False
    sortResultsBy <- obj .:? "sort-by" >>= parseSortBy
    textFileEncoding <- obj .:? "encoding" .!= "utf-8"
    uniqueLines <- obj .:? "uniquelines" .!= False
    verbose <- obj .:? "verbose" .!= False
    return SearchSettings {
      archivesOnly=archivesOnly
    , colorize=colorize
    , debug=debug
    , firstMatch=firstMatch
    , followSymlinks=followSymlinks
    , inArchiveExtensions=inArchiveExtensions
    , inArchiveFilePatterns=inArchiveFilePatterns
    , inDirPatterns=inDirPatterns
    , inExtensions=inExtensions
    , inFilePatterns=inFilePatterns
    , inFileTypes=inFileTypes
    , inLinesAfterPatterns=inLinesAfterPatterns
    , inLinesBeforePatterns=inLinesBeforePatterns
    , includeHidden=includeHidden
    , linesAfter=linesAfter
    , linesAfterToPatterns=linesAfterToPatterns
    , linesAfterUntilPatterns=linesAfterUntilPatterns
    , linesBefore=linesBefore
    , maxDepth=maxDepth
    , maxLastMod=maxLastMod
    , maxLineLength=maxLineLength
    , maxSize=maxSize
    , minDepth=minDepth
    , minLastMod=minLastMod
    , minSize=minSize
    , multiLineSearch=multiLineSearch
    , outArchiveExtensions=outArchiveExtensions
    , outArchiveFilePatterns=outArchiveFilePatterns
    , outDirPatterns=outDirPatterns
    , outExtensions=outExtensions
    , outFilePatterns=outFilePatterns
    , outFileTypes=outFileTypes
    , outLinesAfterPatterns=outLinesAfterPatterns
    , outLinesBeforePatterns=outLinesBeforePatterns
    , paths=paths
    , printDirs=printDirs
    , printFiles=printFiles
    , printLines=printLines
    , printUsage=printUsage
    , printResults=printResults
    , printVersion=printVersion
    , recursive=recursive
    , searchArchives=searchArchives
    , searchPatterns=searchPatterns
    , sortCaseInsensitive=sortCaseInsensitive
    , sortDescending=sortDescending
    , sortResultsBy=sortResultsBy
    , textFileEncoding=textFileEncoding
    , uniqueLines=uniqueLines
    , verbose=verbose
    }

-- Custom function to handle "firstmatch" or "allmatches"
parseFirstMatch :: Object -> Parser Bool
parseFirstMatch obj = do
  let firstMatchField = obj .:? "firstmatch"
      allMatchesField = obj .:? "allmatches"
  firstMatchValue <- firstMatchField
  allMatchesValue <- allMatchesField
  case (firstMatchValue, allMatchesValue) of
    (Just (Bool val), _) -> return val             -- Use "firstmatch" directly if present
    (Nothing, Just (Bool val)) -> return (not val) -- Negate "allmatches" if present
    (Just _, _) -> fail "invalid value for option: firstmatch" -- Invalid "firstmatch" value
    (_, Just _) -> fail "invalid value for option: allmatches" -- Invalid "allmatches" value
    -- (Nothing, Nothing) -> fail "Missing required field: either 'firstmatch' or 'allmatches'"
    (Nothing, Nothing) -> return False

-- Custom function to handle "includehidden" or "excludehidden"
parseIncludeHidden :: Object -> Parser Bool
parseIncludeHidden obj = do
  let includeHiddenField = obj .:? "includehidden"
      excludeHiddenField = obj .:? "excludehidden"
  includeHiddenValue <- includeHiddenField
  excludeHiddenValue <- excludeHiddenField
  case (includeHiddenValue, excludeHiddenValue) of
    (Just (Bool val), _) -> return val             -- Use "includehidden" directly if present
    (Nothing, Just (Bool val)) -> return (not val) -- Negate "excludehidden" if present
    (Just _, _) -> fail "invalid value for option: includehidden" -- Invalid "includehidden" value
    (_, Just _) -> fail "invalid value for option: excludehidden" -- Invalid "excludehidden" value
    -- (Nothing, Nothing) -> fail "Missing required field: either 'includehidden' or 'excludehidden'"
    (Nothing, Nothing) -> return False

-- Custom function to handle "version" or "noversion"
parsePrintVersion :: Object -> Parser Bool
parsePrintVersion obj = do
  let printVersionField = obj .:? "version"
      noPrintVersionField = obj .:? "noversion"
  printVersionValue <- printVersionField
  noPrintVersionValue <- noPrintVersionField
  case (printVersionValue, noPrintVersionValue) of
    (Just (Bool val), _) -> return val             -- Use "version" directly if present
    (Nothing, Just (Bool val)) -> return (not val) -- Negate "noversion" if present
    (Just _, _) -> fail "invalid value for option: version" -- Invalid "version" value
    (_, Just _) -> fail "invalid value for option: noversion" -- Invalid "noversion" value
    -- (Nothing, Nothing) -> fail "Missing required field: either 'includehidden' or 'excludehidden'"
    (Nothing, Nothing) -> return False

-- Helper function to handle string or array of strings
parseStringOrArray :: Maybe Value -> Parser [String]
parseStringOrArray Nothing = return []
parseStringOrArray (Just v) =
  case v of
    String s -> return [unpack s]
    Array a  -> mapM (fmap unpack . parseJSON) (V.toList a)
    _        -> mzero

-- Helper function to parse FileType
parseFileTypes :: Maybe Value -> Parser [FileType]
parseFileTypes Nothing = return []
parseFileTypes (Just v) =
  case v of
    String s -> return [getFileTypeForName (unpack s)]
    Array a  -> mapM (fmap (getFileTypeForName . unpack) . parseJSON) (V.toList a)
    _        -> mzero

-- Helper function to parse optional UTCTime
parseUTCTime :: Maybe Value -> Parser (Maybe UTCTime)
parseUTCTime Nothing = return Nothing
parseUTCTime (Just (String s)) =
  case parseTimeM True defaultTimeLocale "%Y-%m-%dT%H:%M:%S%QZ" (unpack s) of
    Just time -> return (Just time)
    Nothing   -> fail "Invalid UTCTime format"
parseUTCTime _ = mzero

-- Helper function to parse SortBy
parseSortBy :: Maybe Value -> Parser SortBy
parseSortBy Nothing = return SortByFilePath
parseSortBy (Just (String s)) = return $ getSortByForName (unpack s)
parseSortBy _ = mzero

updateSearchSettingsFromJsonValue :: SearchSettings -> Value -> Either String SearchSettings
updateSearchSettingsFromJsonValue settings json =
  case fromJSON json of
    Success newSettings -> Right $ mergeSearchSettings settings newSettings
    Error e             -> Left e

mergeSearchSettings :: SearchSettings -> SearchSettings -> SearchSettings
mergeSearchSettings old new = SearchSettings
  { archivesOnly = archivesOnly new || archivesOnly old
  , colorize = if not (colorize new) then colorize new else colorize old
  , debug = debug new || debug old
  , firstMatch = firstMatch new || firstMatch old
  , followSymlinks = followSymlinks new || followSymlinks old
  , inArchiveExtensions = inArchiveExtensions old ++ inArchiveExtensions new
  , inArchiveFilePatterns = inArchiveFilePatterns old ++ inArchiveFilePatterns new
  , inDirPatterns = inDirPatterns old ++ inDirPatterns new
  , inExtensions = inExtensions old ++ inExtensions new
  , inFilePatterns = inFilePatterns old ++ inFilePatterns new
  , inFileTypes = inFileTypes old ++ inFileTypes new
  , inLinesAfterPatterns = inLinesAfterPatterns old ++ inLinesAfterPatterns new
  , inLinesBeforePatterns = inLinesBeforePatterns old ++ inLinesBeforePatterns new
  , includeHidden = includeHidden new || includeHidden old
  , linesAfter = if linesAfter new > 0 then linesAfter new else linesAfter old
  , linesAfterToPatterns = linesAfterToPatterns old ++ linesAfterToPatterns new
  , linesAfterUntilPatterns = linesAfterUntilPatterns old ++ linesAfterUntilPatterns new
  , linesBefore = if linesBefore new > 0 then linesBefore new else linesBefore old
  , maxDepth = if maxDepth new > -1 then maxDepth new else maxDepth old
  , maxLastMod = if isJust (maxLastMod new) then maxLastMod new else maxLastMod old
  , maxLineLength = if maxLineLength new > 0 then maxLineLength new else maxLineLength old
  , maxSize = if maxSize new > 0 then maxSize new else maxSize old
  , minDepth = if minDepth new > -1 then minDepth new else minDepth old
  , minLastMod = if isJust (minLastMod new) then minLastMod new else minLastMod old
  , minSize = if minSize new > 0 then minSize new else minSize old
  , multiLineSearch = multiLineSearch new || multiLineSearch old
  , outArchiveExtensions = outArchiveExtensions old ++ outArchiveExtensions new
  , outArchiveFilePatterns = outArchiveFilePatterns old ++ outArchiveFilePatterns new
  , outDirPatterns = outDirPatterns old ++ outDirPatterns new
  , outExtensions = outExtensions old ++ outExtensions new
  , outFilePatterns = outFilePatterns old ++ outFilePatterns new
  , outFileTypes = outFileTypes old ++ outFileTypes new
  , outLinesAfterPatterns = outLinesAfterPatterns old ++ outLinesAfterPatterns new
  , outLinesBeforePatterns = outLinesBeforePatterns old ++ outLinesBeforePatterns new
  , paths = paths old ++ paths new
  , printDirs = printDirs new || printDirs old
  , printFiles = printFiles new || printFiles old
  , printLines = printLines new || printLines old
  , printResults = printResults new || printResults old
  , printUsage = printUsage new || printUsage old
  , printVersion = printVersion new || printVersion old
    -- recursive is true by default, so a false value overrides
  , recursive = if not (recursive new) then recursive new else recursive old
  , searchArchives = searchArchives new || searchArchives old
  , searchPatterns = searchPatterns old ++ searchPatterns new
  , sortCaseInsensitive = sortCaseInsensitive new || sortCaseInsensitive old
  , sortDescending = sortDescending new || sortDescending old
  , sortResultsBy = if sortResultsBy new /= SortByFilePath then sortResultsBy new else sortResultsBy old
  , textFileEncoding = if textFileEncoding new /= "utf-9" then textFileEncoding new else textFileEncoding old
  , uniqueLines = uniqueLines new || uniqueLines old
  , verbose = verbose new || verbose old
  }
