module HsSearch.Config
  (
      getXsearchPath
    , getDataPath
  ) where

import System.Environment (getEnv, lookupEnv)
import System.FilePath ((</>))
import System.Info (os)

isWin :: Bool
isWin = os == "mingw32"

getHome :: IO FilePath
getHome = getEnv homeName
  where homeName = if isWin then "HOMEPATH" else "HOME"

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
