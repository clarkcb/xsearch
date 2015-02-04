{-# LANGUAGE Arrows, NoMonomorphismRestriction #-}
module HsSearch.SearchOptions (
    SearchOption(..)
  , getSearchOptions
  , getUsage
  , settingsFromArgs) where

import Data.Char (toLower)
import Data.List (isPrefixOf, sortBy)
import Data.List.Split (splitOn)
import Data.Maybe (isJust)
import qualified Data.Text as T
import Text.XML.HXT.Core

import HsSearch.FileUtil (normalizeExtension)
import HsSearch.Paths_hssearch (getDataFileName)
import HsSearch.SearchSettings

data SearchOption = SearchOption { long, short, desc :: String }
  deriving (Show, Eq)

searchOptionsFile :: FilePath
searchOptionsFile = "searchoptions.xml"

atTag :: ArrowXml a => String -> a XmlTree XmlTree
atTag tag = deep (isElem >>> hasName tag)

strip :: String -> String
strip = T.unpack . T.strip . T.pack

getSearchOption :: IOSLA (XIOState ()) XmlTree SearchOption
getSearchOption = atTag "searchoption" >>>
  proc f -> do
    l <- getAttrValue "long" -< f
    s <- getAttrValue "short" -< f
    d <- getChildren >>> getText -< f
    returnA -< SearchOption { long = l, short = s, desc = strip d }

getSearchOptions :: IO [SearchOption]
getSearchOptions = do
  searchOptionsPath <- getDataFileName searchOptionsFile
  runX (readDocument [withValidate no] searchOptionsPath
    >>> getSearchOption)

getUsage :: [SearchOption] -> String
getUsage searchOptions =
  "Usage:\n  hssearch [options] <startpath>\n\nOptions:\n" ++
  searchOptionsToString searchOptions

getOptStrings :: [SearchOption] -> [String]
getOptStrings searchOptions = 
  map formatOpts searchOptions
  where formatOpts SearchOption {long=l, short=""} = getLong l
        formatOpts SearchOption {long=l, short=s}  = shortAndLong s l
        getLong l = "--" ++ l
        shortAndLong s l = "-" ++ s ++ "," ++ getLong l

getOptDesc :: SearchOption -> String
getOptDesc SearchOption {desc=""} = error "No description for SearchOption"
getOptDesc SearchOption {desc=d} = d

sortSearchOption :: SearchOption -> SearchOption -> Ordering
sortSearchOption (SearchOption {long=l1, short=s1}) (SearchOption {long=l2, short=s2}) =
  compare (shortOrLong s1 l1) (shortOrLong s2 l2)
  where
    shortOrLong "" l = l
    shortOrLong s l = (map toLower s) ++ "@" ++ l

sortSearchOptions :: [SearchOption] -> [SearchOption]
sortSearchOptions searchOptions = sortBy sortSearchOption searchOptions

padString :: String -> Int -> String
padString s len | length s < len = s ++ take (len - length s) (repeat ' ')
                | otherwise      = s

searchOptionsToString :: [SearchOption] -> String
searchOptionsToString searchOptions = 
  unlines $ zipWith formatOptLine optStrings optDescs
  where
    sorted = sortSearchOptions searchOptions
    optStrings = getOptStrings sorted
    optDescs = map getOptDesc sorted
    longest = maximum $ map length optStrings
    formatOptLine o d = "  " ++ (padString o longest) ++ "  " ++ d

data ActionType = ArgActionType
                | FlagActionType
                | UnknownActionType
  deriving (Show, Eq)

type ArgAction = SearchSettings -> String -> SearchSettings
type FlagAction = SearchSettings -> SearchSettings

argActions :: [(String, ArgAction)]
argActions = [ ("in-archiveext", \ss s -> ss {inArchiveExtensions = inArchiveExtensions ss ++ (newExtensions s)})
             , ("in-archivefilepattern", \ss s -> ss {inArchiveFilePatterns = inArchiveFilePatterns ss ++ [s]})
             , ("in-dirpattern", \ss s -> ss {inDirPatterns = inDirPatterns ss ++ [s]})
             , ("in-ext", \ss s -> ss {inExtensions = inExtensions ss ++ (newExtensions s)})
             , ("in-filepattern", \ss s -> ss {inFilePatterns = inFilePatterns ss ++ [s]})
             , ("in-linesafterpattern", \ss s -> ss {inLinesAfterPatterns = inLinesAfterPatterns ss ++ [s]})
             , ("in-linesbeforepattern", \ss s -> ss {inLinesBeforePatterns = inLinesBeforePatterns ss ++ [s]})
             , ("linesafter", \ss s -> ss {linesAfter = read s})
             , ("linesaftertopattern", \ss s -> ss {linesAfterToPatterns = linesAfterToPatterns ss ++ [s]})
             , ("linesafteruntilpattern", \ss s -> ss {linesAfterUntilPatterns = linesAfterUntilPatterns ss ++ [s]})
             , ("linesbefore", \ss s -> ss {linesBefore = read s})
             , ("maxlinelength", \ss s -> ss {maxLineLength = read s})
             , ("out-archiveext", \ss s -> ss {outArchiveExtensions = outArchiveExtensions ss ++ (newExtensions s)})
             , ("out-archivefilepattern", \ss s -> ss {outArchiveFilePatterns = outArchiveFilePatterns ss ++ [s]})
             , ("out-dirpattern", \ss s -> ss {outDirPatterns = outDirPatterns ss ++ [s]})
             , ("out-ext", \ss s -> ss {outExtensions = outExtensions ss ++ (newExtensions s)})
             , ("out-filepattern", \ss s -> ss {outFilePatterns = outFilePatterns ss ++ [s]})
             , ("out-linesafterpattern", \ss s -> ss {outLinesAfterPatterns = outLinesAfterPatterns ss ++ [s]})
             , ("out-linesbeforepattern", \ss s -> ss {outLinesBeforePatterns = outLinesBeforePatterns ss ++ [s]})
             , ("search", \ss s -> ss {searchPatterns = searchPatterns ss ++ [s]})
             ]

newExtensions :: String -> [String]
newExtensions x | ',' `elem` x = map normalizeExtension $ removeBlank (splitOn "," x)
                | otherwise    = [normalizeExtension x]
  where removeBlank :: [String] -> [String]
        removeBlank = filter (/="")

flagActions :: [(String, FlagAction)]
flagActions = [ ("allmatches", \ss -> ss {firstMatch=False})
              , ("archivesonly", \ss -> ss {archivesOnly=True,
                                            searchArchives=True})
              , ("debug", \ss -> ss {debug=True, verbose=True})
              , ("dotiming", \ss -> ss {doTiming=True})
              , ("excludehidden", \ss -> ss {excludeHidden=True})
              , ("firstmatch", \ss -> ss {firstMatch=True})
              , ("help", \ss -> ss {printUsage=True})
              , ("includehidden", \ss -> ss {excludeHidden=False})
              , ("listdirs", \ss -> ss {listDirs=True})
              , ("listfiles", \ss -> ss {listFiles=True})
              , ("listlines", \ss -> ss {listLines=True})
              , ("multilinesearch", \ss -> ss {multiLineSearch=True})
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

shortToLong :: [SearchOption] -> String -> String
shortToLong _ "" = ""
shortToLong opts s | length s == 2 && head s == '-' = "--" ++ getLongForShort s
                   | otherwise = s
  where getLongForShort x = (long . head . filter (\so -> short so == tail x)) opts

settingsFromArgs :: [SearchOption] -> [String] -> SearchSettings
settingsFromArgs opts arguments =
  recSettingsFromArgs defaultSearchSettings $ map (shortToLong opts) arguments
  where recSettingsFromArgs :: SearchSettings -> [String] -> SearchSettings
        recSettingsFromArgs settings args =
          case args of
          [] -> settings
          a:as | "-" `isPrefixOf` a ->
            case getActionType (argName a) of
            ArgActionType -> recSettingsFromArgs ((getArgAction (argName a)) settings (head as)) (tail as)
            FlagActionType -> recSettingsFromArgs ((getFlagAction (argName a)) settings) as
            UnknownActionType -> error ("Unknown option: " ++ (argName a))
          a:as -> recSettingsFromArgs (settings {startPath=a}) as
        getActionType :: String -> ActionType
        getActionType a = if (isArgAction a)
                          then ArgActionType
                          else if (isFlagAction a)
                               then FlagActionType
                               else UnknownActionType
        argName :: String -> String
        argName a = (dropWhile (=='-') a)
        getArgAction ::String -> ArgAction
        getArgAction a = snd $ head $ filter (\(x,_) -> a==x) argActions
        getFlagAction ::String -> FlagAction
        getFlagAction a = snd $ head $ filter (\(x,_) -> a==x) flagActions
        isArgAction ::String -> Bool
        isArgAction a = isJust $ lookup a argActions
        isFlagAction ::String -> Bool
        isFlagAction a = isJust $ lookup a flagActions
