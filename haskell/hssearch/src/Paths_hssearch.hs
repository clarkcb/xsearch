module Paths_hssearch where

import System.FilePath ((</>))

-- NOTE: this path is only used for testing/development, after cabal install
-- the path will be overridden by the data-files setting in the cabal file
dataFilePath :: FilePath
dataFilePath = "/Users/cary/src/git/xsearch/haskell/hssearch/data"

getDataFileName :: FilePath -> IO FilePath
getDataFileName f = return $ dataFilePath </> f
