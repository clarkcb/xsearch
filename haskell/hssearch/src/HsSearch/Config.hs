module HsSearch.Config
  ( getDataPath ) where

import System.Environment (getEnv)
import System.FilePath ((</>))
import System.Info (os)

isWin :: Bool
isWin = os == "mingw32"

getHome :: IO FilePath
getHome = getEnv homeName
  where homeName = if isWin then "HOMEPATH" else "HOME"

getDataPath :: IO FilePath
getDataPath = do
  home <- getHome
  let elems = ["src", "git", "xsearch", "haskell", "hssearch", "data"]
  return $ foldl concatPath home elems
  where concatPath path p = path </> p  
