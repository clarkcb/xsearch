module HsSearch.SearchConfig
  ( SearchConfig(..)
    , getSearchConfig
    , getXsearchPath
    , getSearchDataPath
    , getDefaultSearchSettingsPath
  ) where

import System.Directory (getHomeDirectory)
import System.Environment (lookupEnv)
import System.FilePath ((</>))
import System.Info (os)
import HsFind.FileUtil (concatPath)
import HsFind.FindConfig

isWin :: Bool
isWin = os == "mingw32"

data SearchConfig = SearchConfig {
    findConfig :: FindConfig
  , xsearchPath :: FilePath
  , searchOptionsPath :: FilePath
  , defaultSearchSettingsPath :: FilePath
  } deriving (Show, Eq)

getSearchConfig :: IO SearchConfig
getSearchConfig = do
  findConfig <- getFindConfig
  xsearchPath <- getXsearchPath
  dataPath <- getSearchDataPath
  let searchOptionsPath = concatPath dataPath "searchoptions.json"
  defaultSearchSettingsPath <- getDefaultSearchSettingsPath
  return SearchConfig {
    findConfig=findConfig
  , xsearchPath = xsearchPath
  , searchOptionsPath = searchOptionsPath
  , defaultSearchSettingsPath = defaultSearchSettingsPath
  }

defaultXSearchConfigDir :: IO FilePath
defaultXSearchConfigDir = do
  home <- getHomeDirectory
  return $ foldl concatPath home [".config", "xsearch"]

getXsearchConfigDir :: IO FilePath
getXsearchConfigDir = do
  home <- getHomeDirectory
  maybeXsearchConfigDir <- lookupEnv "XSEARCH_CONFIG_DIR"
  case maybeXsearchConfigDir of
    Just xsearchConfigDir -> return xsearchConfigDir
    Nothing -> defaultXSearchConfigDir

defaultXsearchPath :: IO FilePath
defaultXsearchPath = do
  home <- getHomeDirectory
  return $ foldl concatPath home ["src", "xsearch"]

getXsearchPath :: IO FilePath
getXsearchPath = do
  home <- getHomeDirectory
  maybeXsearchPath <- lookupEnv "XSEARCH_PATH"
  case maybeXsearchPath of
    Just xsearchPath -> return xsearchPath
    Nothing -> defaultXsearchPath

getSearchDataPath :: IO FilePath
getSearchDataPath = do
  xsearchPath <- getXsearchPath
  let elems = ["haskell", "hssearch", "data"]
  return $ foldl concatPath xsearchPath elems
  where concatPath path p = path </> p  

getDefaultSearchSettingsPath :: IO FilePath
getDefaultSearchSettingsPath = do
  xsearchConfigDir <- getXsearchConfigDir
  let elems = ["settings.json"]
  return $ foldl concatPath xsearchConfigDir elems
  where concatPath path p = path </> p  
