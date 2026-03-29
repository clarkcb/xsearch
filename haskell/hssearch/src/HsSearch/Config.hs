module HsSearch.Config
  (
      getXsearchPath
    , getDataPath
    , getDefaultSearchSettingsPath
  ) where

import System.Environment (lookupEnv)
import System.FilePath ((</>))
import System.Info (os)
import HsFind.Config (getHome)

isWin :: Bool
isWin = os == "mingw32"

getXsearchPath :: IO FilePath
getXsearchPath = do
  home <- getHome
  maybeXsearchPath <- lookupEnv "XSEARCH_PATH"
  case maybeXsearchPath of
    Just xsearchPath -> return xsearchPath
    Nothing -> return $ home ++ "/src/xsearch"

getDataPath :: IO FilePath
getDataPath = do
  xsearchPath <- getXsearchPath
  let elems = ["haskell", "hssearch", "data"]
  return $ foldl concatPath xsearchPath elems
  where concatPath path p = path </> p  

getDefaultSearchSettingsPath :: IO FilePath
getDefaultSearchSettingsPath = do
  home <- getHome
  let elems = [".config", "xsearch", "settings.json"]
  return $ foldl concatPath home elems
  where concatPath path p = path </> p  
