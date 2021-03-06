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

import HsSearch.Paths_hssearch (getDataFileName)
import HsSearch.FileTypes (getFileTypeForName)
import HsSearch.FileUtil (getFileString)
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
  "Usage:\n hssearch [options] -s <searchpattern> <startpath>\n\nOptions:\n" ++
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

data ActionType = ArgActionType
                | BoolFlagActionType
                | FlagActionType
                | UnknownActionType
  deriving (Show, Eq)

type ArgAction = SearchSettings -> String -> SearchSettings
type BoolFlagAction = SearchSettings -> Bool -> SearchSettings
type FlagAction = SearchSettings -> SearchSettings

argActions :: [(String, ArgAction)]
argActions = [ ("encoding", \ss s -> ss {textFileEncoding=s})
             , ("in-archiveext", \ss s -> ss {inArchiveExtensions = inArchiveExtensions ss ++ newExtensions s})
             , ("in-archivefilepattern", \ss s -> ss {inArchiveFilePatterns = inArchiveFilePatterns ss ++ [s]})
             , ("in-dirpattern", \ss s -> ss {inDirPatterns = inDirPatterns ss ++ [s]})
             , ("in-ext", \ss s -> ss {inExtensions = inExtensions ss ++ newExtensions s})
             , ("in-filepattern", \ss s -> ss {inFilePatterns = inFilePatterns ss ++ [s]})
             , ("in-filetype", \ss s -> ss {inFileTypes = inFileTypes ss ++ [getFileTypeForName s]})
             , ("in-linesafterpattern", \ss s -> ss {inLinesAfterPatterns = inLinesAfterPatterns ss ++ [s]})
             , ("in-linesbeforepattern", \ss s -> ss {inLinesBeforePatterns = inLinesBeforePatterns ss ++ [s]})
             , ("linesafter", \ss s -> ss {linesAfter = read s})
             , ("linesaftertopattern", \ss s -> ss {linesAfterToPatterns = linesAfterToPatterns ss ++ [s]})
             , ("linesafteruntilpattern", \ss s -> ss {linesAfterUntilPatterns = linesAfterUntilPatterns ss ++ [s]})
             , ("linesbefore", \ss s -> ss {linesBefore = read s})
             , ("maxlinelength", \ss s -> ss {maxLineLength = read s})
             , ("out-archiveext", \ss s -> ss {outArchiveExtensions = outArchiveExtensions ss ++ newExtensions s})
             , ("out-archivefilepattern", \ss s -> ss {outArchiveFilePatterns = outArchiveFilePatterns ss ++ [s]})
             , ("out-dirpattern", \ss s -> ss {outDirPatterns = outDirPatterns ss ++ [s]})
             , ("out-ext", \ss s -> ss {outExtensions = outExtensions ss ++ newExtensions s})
             , ("out-filepattern", \ss s -> ss {outFilePatterns = outFilePatterns ss ++ [s]})
             , ("out-filetype", \ss s -> ss {outFileTypes = outFileTypes ss ++ [getFileTypeForName s]})
             , ("out-linesafterpattern", \ss s -> ss {outLinesAfterPatterns = outLinesAfterPatterns ss ++ [s]})
             , ("out-linesbeforepattern", \ss s -> ss {outLinesBeforePatterns = outLinesBeforePatterns ss ++ [s]})
             , ("searchpattern", \ss s -> ss {searchPatterns = searchPatterns ss ++ [s]})
             ]

flagActions :: [(String, FlagAction)]
flagActions = [ ("allmatches", \ss -> ss {firstMatch=False})
              , ("archivesonly", \ss -> ss {archivesOnly=True,
                                            searchArchives=True})
              , ("colorize", \ss -> ss {colorize=True})
              , ("debug", \ss -> ss {debug=True, verbose=True})
              , ("excludehidden", \ss -> ss {excludeHidden=True})
              , ("firstmatch", \ss -> ss {firstMatch=True})
              , ("help", \ss -> ss {printUsage=True})
              , ("includehidden", \ss -> ss {excludeHidden=False})
              , ("listdirs", \ss -> ss {listDirs=True})
              , ("listfiles", \ss -> ss {listFiles=True})
              , ("listlines", \ss -> ss {listLines=True})
              , ("multilinesearch", \ss -> ss {multiLineSearch=True})
              , ("nocolorize", \ss -> ss {colorize=False})
              , ("noprintmatches", \ss -> ss {printResults=False})
              , ("norecursive", \ss -> ss {recursive=False})
              , ("nosearcharchives", \ss -> ss {searchArchives=False})
              , ("printmatches", \ss -> ss {printResults=True})
              , ("recursive", \ss -> ss {recursive=True})
              , ("searcharchives", \ss -> ss {searchArchives=True})
              , ("uniquelines", \ss -> ss {uniqueLines=True})
              , ("verbose", \ss -> ss {verbose=True})
              , ("version", \ss -> ss {printVersion=True})
              ]

boolFlagActions :: [(String, BoolFlagAction)]
boolFlagActions = [ ("allmatches", \ss b -> ss {firstMatch=not b})
                  , ("archivesonly", \ss b -> ss {archivesOnly=b,
                                                  searchArchives=b})
                  , ("colorize", \ss b -> ss {colorize=b})
                  , ("debug", \ss b -> ss {debug=b, verbose=b})
                  , ("excludehidden", \ss b -> ss {excludeHidden=b})
                  , ("firstmatch", \ss b -> ss {firstMatch=b})
                  , ("help", \ss b -> ss {printUsage=b})
                  , ("includehidden", \ss b -> ss {excludeHidden=not b})
                  , ("listdirs", \ss b -> ss {listDirs=b})
                  , ("listfiles", \ss b -> ss {listFiles=b})
                  , ("listlines", \ss b -> ss {listLines=b})
                  , ("multilinesearch", \ss b -> ss {multiLineSearch=b})
                  , ("nocolorize", \ss b -> ss {colorize=not b})
                  , ("noprintmatches", \ss b -> ss {printResults=not b})
                  , ("norecursive", \ss b -> ss {recursive=not b})
                  , ("nosearcharchives", \ss b -> ss {searchArchives=not b})
                  , ("printmatches", \ss b -> ss {printResults=b})
                  , ("recursive", \ss b -> ss {recursive=b})
                  , ("searcharchives", \ss b -> ss {searchArchives=b})
                  , ("uniquelines", \ss b -> ss {uniqueLines=b})
                  , ("verbose", \ss b -> ss {verbose=b})
                  , ("version", \ss b -> ss {printVersion=b})
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

settingsFromArgs :: [SearchOption] -> [String] -> Either String SearchSettings
settingsFromArgs opts arguments =
  if any isLeft longArgs
  then (Left . head . lefts) longArgs
  else
    recSettingsFromArgs defaultSearchSettings $ rights longArgs
  where recSettingsFromArgs :: SearchSettings -> [String] -> Either String SearchSettings
        recSettingsFromArgs settings args =
          case args of
          [] -> Right settings
          [a] | "-" `isPrefixOf` a ->
            case getActionType (argName a) of
              ArgActionType -> Left $ "Missing value for option: " ++ a ++ "\n"
              BoolFlagActionType -> recSettingsFromArgs (getBoolFlagAction (argName a) settings True) []
              FlagActionType -> recSettingsFromArgs (getFlagAction (argName a) settings) []
              UnknownActionType -> Left $ "Invalid option: " ++ a ++ "\n"
          a:as | "-" `isPrefixOf` a ->
            case getActionType (argName a) of
              ArgActionType -> recSettingsFromArgs (getArgAction (argName a) settings (head as)) (tail as)
              BoolFlagActionType -> recSettingsFromArgs (getBoolFlagAction (argName a) settings True) as
              FlagActionType -> recSettingsFromArgs (getFlagAction (argName a) settings) as
              UnknownActionType -> Left $ "Invalid option: " ++ argName a ++ "\n"
          a:as -> recSettingsFromArgs (settings {startPath=a}) as
        longArgs :: [Either String String]
        longArgs = map (shortToLong opts) arguments
        getActionType :: String -> ActionType
        getActionType a
          | isArgAction a = ArgActionType
          | isBoolFlagAction a = BoolFlagActionType
          | isFlagAction a = FlagActionType
          | otherwise = UnknownActionType
        argName :: String -> String
        argName = dropWhile (=='-')
        getArgAction :: String -> ArgAction
        getArgAction a = snd $ head $ filter (\(x,_) -> a==x) argActions
        getBoolFlagAction :: String -> BoolFlagAction
        getBoolFlagAction a = snd $ head $ filter (\(x,_) -> a==x) boolFlagActions
        getFlagAction :: String -> FlagAction
        getFlagAction a = snd $ head $ filter (\(x,_) -> a==x) flagActions
        isArgAction :: String -> Bool
        isArgAction a = isJust $ lookup a argActions
        isBoolFlagAction :: String -> Bool
        isBoolFlagAction a = isJust $ lookup a boolFlagActions
        isFlagAction :: String -> Bool
        isFlagAction a = isJust $ lookup a flagActions
