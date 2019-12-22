module HsSearch.Config
  (
      getXsearchPath
    , getDataPath
  ) where

import System.Environment (getEnv)
import System.FilePath ((</>))
import System.Info (os)

isWin :: Bool
isWin = os == "mingw32"

getHome :: IO FilePath
getHome = getEnv homeName
  where homeName = if isWin then "HOMEPATH" else "HOME"

xsearchPath :: FilePath
xsearchPath = "/Users/cary/src/xsearch"

getXsearchPath :: IO FilePath
getXsearchPath =
  return xsearchPath

getDataPath :: IO FilePath
getDataPath = do
  let elems = ["haskell", "hssearch", "data"]
  return $ foldl concatPath xsearchPath elems
  where concatPath path p = path </> p  
