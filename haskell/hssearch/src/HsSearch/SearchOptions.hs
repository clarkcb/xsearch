{-# LANGUAGE DeriveGeneric, NoMonomorphismRestriction #-}
module HsSearch.SearchOptions (
    SearchOption(..)
  , getSearchOptions
  , getUsage
  , settingsFromArgs) where

import qualified Data.ByteString.Lazy.Char8 as BC
import Data.Char (toLower)
import Data.Either (isLeft, lefts, rights)
import Data.List (isPrefixOf, sortBy)
import Data.Maybe (isJust)

import GHC.Generics
import Data.Aeson

import HsFind.FileTypes (getFileTypeForName)
import HsFind.FileUtil (getFileString)
import HsFind.FindOptions (parseDateToUtc)
import HsFind.SortBy (getSortByForName)

import HsSearch.Paths_hssearch (getDataFileName)
import HsSearch.SearchSettings

data SearchOption = SearchOption
  { long :: String
  , short :: Maybe String
  , desc :: String
  } deriving (Show, Eq, Generic)

instance FromJSON SearchOption

newtype SearchOptions
  = SearchOptions {searchoptions :: [SearchOption]}
  deriving (Show, Eq, Generic)

instance FromJSON SearchOptions

searchOptionsFile :: FilePath
searchOptionsFile = "searchoptions.json"

getSearchOptions :: IO [SearchOption]
getSearchOptions = do
  searchOptionsPath <- getDataFileName searchOptionsFile
  searchOptionsJsonString <- getFileString searchOptionsPath
  case searchOptionsJsonString of
    (Left _) -> return []
    (Right jsonString) ->
      case (eitherDecode (BC.pack jsonString) :: Either String SearchOptions) of
        (Left e) -> return [SearchOption {long=e, short=Nothing, desc=e}]
        (Right jsonSearchOptions) -> return (searchoptions jsonSearchOptions)

getUsage :: [SearchOption] -> String
getUsage searchOptions =
  "Usage:\n hssearch [options] -s <searchpattern> <path> [<path> ...]\n\nOptions:\n" ++
  searchOptionsToString searchOptions

getOptStrings :: [SearchOption] -> [String]
getOptStrings = map formatOpts
  where formatOpts SearchOption {long=l, short=Nothing} = getLong l
        formatOpts SearchOption {long=l, short=Just s}  = shortAndLong s l
        getLong l = "--" ++ l
        shortAndLong s l = "-" ++ s ++ "," ++ getLong l

getOptDesc :: SearchOption -> String
getOptDesc SearchOption {desc=""} = error "No description for SearchOption"
getOptDesc SearchOption {desc=d} = d

sortSearchOption :: SearchOption -> SearchOption -> Ordering
sortSearchOption SearchOption {long=l1, short=s1} SearchOption {long=l2, short=s2} =
  compare (shortOrLong s1 l1) (shortOrLong s2 l2)
  where
    shortOrLong Nothing l = l
    shortOrLong (Just s) l = map toLower s ++ "@" ++ l

sortSearchOptions :: [SearchOption] -> [SearchOption]
sortSearchOptions = sortBy sortSearchOption

padString :: String -> Int -> String
padString s len | length s < len = s ++ replicate (len - length s) ' '
                | otherwise      = s

searchOptionsToString :: [SearchOption] -> String
searchOptionsToString searchOptions = 
  unlines $ zipWith formatOptLine optStrings optDescs
  where
    sorted = sortSearchOptions searchOptions
    optStrings = getOptStrings sorted
    optDescs = map getOptDesc sorted
    longest = maximum $ map length optStrings
    formatOptLine o d = " " ++ padString o longest ++ "  " ++ d

data ActionType = BoolActionType
                | StringActionType
                | IntegerActionType
                | UnknownActionType
  deriving (Show, Eq)

type BoolAction = SearchSettings -> Bool -> SearchSettings
type StringAction = SearchSettings -> String -> SearchSettings
type IntegerAction = SearchSettings -> Integer -> SearchSettings

boolActions :: [(String, BoolAction)]
boolActions = [ ("allmatches", \ss b -> ss {firstMatch=not b})
              , ("archivesonly", \ss b -> ss {archivesOnly=b,
                                              searchArchives=b})
              , ("colorize", \ss b -> ss {colorize=b})
              , ("debug", \ss b -> ss {debug=b, verbose=b})
              , ("excludehidden", \ss b -> ss {includeHidden=not b})
              , ("firstmatch", \ss b -> ss {firstMatch=b})
              , ("followsymlinks", \ss b -> ss {followSymlinks=b})
              , ("help", \ss b -> ss {printUsage=b})
              , ("includehidden", \ss b -> ss {includeHidden=b})
              , ("multilinesearch", \ss b -> ss {multiLineSearch=b})
              , ("nocolorize", \ss b -> ss {colorize=not b})
              , ("nofollowsymlinks", \ss b -> ss {followSymlinks=not b})
              , ("noprintdirs", \ss b -> ss {printDirs=not b})
              , ("noprintfiles", \ss b -> ss {printFiles=not b})
              , ("noprintlines", \ss b -> ss {printLines=not b})
              , ("noprintmatches", \ss b -> ss {printResults=not b})
              , ("norecursive", \ss b -> ss {recursive=not b})
              , ("nosearcharchives", \ss b -> ss {searchArchives=not b})
              , ("printdirs", \ss b -> ss {printDirs=b})
              , ("printfiles", \ss b -> ss {printFiles=b})
              , ("printlines", \ss b -> ss {printLines=b})
              , ("printmatches", \ss b -> ss {printResults=b})
              , ("recursive", \ss b -> ss {recursive=b})
              , ("searcharchives", \ss b -> ss {searchArchives=b})
              , ("sort-ascending", \ss b -> ss {sortDescending=not b})
              , ("sort-caseinsensitive", \ss b -> ss {sortCaseInsensitive=b})
              , ("sort-casesensitive", \ss b -> ss {sortCaseInsensitive=not b})
              , ("sort-descending", \ss b -> ss {sortDescending=b})
              , ("uniquelines", \ss b -> ss {uniqueLines=b})
              , ("verbose", \ss b -> ss {verbose=b})
              , ("version", \ss b -> ss {printVersion=b})
              ]

stringActions :: [(String, StringAction)]
stringActions = [ ("encoding", \ss s -> ss {textFileEncoding=s})
                , ("in-archiveext", \ss s -> ss {inArchiveExtensions = inArchiveExtensions ss ++ newExtensions s})
                , ("in-archivefilepattern", \ss s -> ss {inArchiveFilePatterns = inArchiveFilePatterns ss ++ [s]})
                , ("in-dirpattern", \ss s -> ss {inDirPatterns = inDirPatterns ss ++ [s]})
                , ("in-ext", \ss s -> ss {inExtensions = inExtensions ss ++ newExtensions s})
                , ("in-filepattern", \ss s -> ss {inFilePatterns = inFilePatterns ss ++ [s]})
                , ("in-filetype", \ss s -> ss {inFileTypes = inFileTypes ss ++ [getFileTypeForName s]})
                , ("in-linesafterpattern", \ss s -> ss {inLinesAfterPatterns = inLinesAfterPatterns ss ++ [s]})
                , ("in-linesbeforepattern", \ss s -> ss {inLinesBeforePatterns = inLinesBeforePatterns ss ++ [s]})
                , ("linesaftertopattern", \ss s -> ss {linesAfterToPatterns = linesAfterToPatterns ss ++ [s]})
                , ("linesafteruntilpattern", \ss s -> ss {linesAfterUntilPatterns = linesAfterUntilPatterns ss ++ [s]})
                , ("maxlastmod", \ss s -> ss {maxLastMod = parseDateToUtc s})
                , ("minlastmod", \ss s -> ss {minLastMod = parseDateToUtc s})
                , ("out-archiveext", \ss s -> ss {outArchiveExtensions = outArchiveExtensions ss ++ newExtensions s})
                , ("out-archivefilepattern", \ss s -> ss {outArchiveFilePatterns = outArchiveFilePatterns ss ++ [s]})
                , ("out-dirpattern", \ss s -> ss {outDirPatterns = outDirPatterns ss ++ [s]})
                , ("out-ext", \ss s -> ss {outExtensions = outExtensions ss ++ newExtensions s})
                , ("out-filepattern", \ss s -> ss {outFilePatterns = outFilePatterns ss ++ [s]})
                , ("out-filetype", \ss s -> ss {outFileTypes = outFileTypes ss ++ [getFileTypeForName s]})
                , ("out-linesafterpattern", \ss s -> ss {outLinesAfterPatterns = outLinesAfterPatterns ss ++ [s]})
                , ("out-linesbeforepattern", \ss s -> ss {outLinesBeforePatterns = outLinesBeforePatterns ss ++ [s]})
                , ("path", \ss s -> ss {paths = paths ss ++ [s]})
                , ("searchpattern", \ss s -> ss {searchPatterns = searchPatterns ss ++ [s]})
                , ("sort-by", \ss s -> ss {sortResultsBy = getSortByForName s})
                ]

integerActions :: [(String, IntegerAction)]
integerActions = [ ("linesafter", \ss i -> ss {linesAfter = i})
                 , ("linesbefore", \ss i -> ss {linesBefore = i})
                 , ("maxdepth", \ss i -> ss {maxDepth = i})
                 , ("maxlinelength", \ss i -> ss {maxLineLength = i})
                 , ("maxsize", \ss i -> ss {maxSize = i})
                 , ("mindepth", \ss i -> ss {minDepth = i})
                 , ("minsize", \ss i -> ss {minSize = i})
                 ]

shortToLong :: [SearchOption] -> String -> Either String String
shortToLong _ "" = Left "Missing argument"
shortToLong opts s | length s == 2 && head s == '-' =
                      if any (\so -> short so == Just (tail s)) optsWithShort
                      then Right $ "--" ++ getLongForShort s
                      else Left $ "Invalid option: " ++ tail s ++ "\n"
                   | otherwise = Right s
  where optsWithShort = filter (isJust . short) opts
        getLongForShort x = (long . head . filter (\so -> short so == Just (tail x))) optsWithShort

updateSettingsFromArgs :: SearchSettings -> [SearchOption] -> [String] -> Either String SearchSettings
updateSettingsFromArgs settings opts arguments =
  if any isLeft longArgs
  then (Left . head . lefts) longArgs
  else
    recSettingsFromArgs settings $ rights longArgs
  where recSettingsFromArgs :: SearchSettings -> [String] -> Either String SearchSettings
        recSettingsFromArgs ss args =
          case args of
          [] -> Right ss
          [a] | "-" `isPrefixOf` a ->
            case getActionType (argName a) of
              BoolActionType -> recSettingsFromArgs (getBoolAction (argName a) ss True) []
              StringActionType -> Left $ "Missing value for option: " ++ a ++ "\n"
              IntegerActionType -> Left $ "Missing value for option: " ++ a ++ "\n"
              UnknownActionType -> Left $ "Invalid option: " ++ a ++ "\n"
          a:as | "-" `isPrefixOf` a ->
            case getActionType (argName a) of
              BoolActionType -> recSettingsFromArgs (getBoolAction (argName a) ss True) as
              StringActionType -> recSettingsFromArgs (getStringAction (argName a) ss (head as)) (tail as)
              IntegerActionType -> recSettingsFromArgs (getIntegerAction (argName a) ss (read (head as))) (tail as)
              UnknownActionType -> Left $ "Invalid option: " ++ argName a ++ "\n"
          a:as -> recSettingsFromArgs (ss {paths = paths ss ++ [a]}) as
        longArgs :: [Either String String]
        longArgs = map (shortToLong opts) arguments
        getActionType :: String -> ActionType
        getActionType a
          | isBoolAction a = BoolActionType
          | isStringAction a = StringActionType
          | isIntegerAction a = IntegerActionType
          | otherwise = UnknownActionType
        argName :: String -> String
        argName = dropWhile (=='-')
        getBoolAction :: String -> BoolAction
        getBoolAction a = snd $ head $ filter (\(x,_) -> a==x) boolActions
        getStringAction :: String -> StringAction
        getStringAction a = snd $ head $ filter (\(x,_) -> a==x) stringActions
        getIntegerAction :: String -> IntegerAction
        getIntegerAction a = snd $ head $ filter (\(x,_) -> a==x) integerActions
        isBoolAction :: String -> Bool
        isBoolAction a = isJust $ lookup a boolActions
        isStringAction :: String -> Bool
        isStringAction a = isJust $ lookup a stringActions
        isIntegerAction :: String -> Bool
        isIntegerAction a = isJust $ lookup a integerActions

settingsFromArgs :: [SearchOption] -> [String] -> Either String SearchSettings
settingsFromArgs = updateSettingsFromArgs defaultSearchSettings{printResults=True}
