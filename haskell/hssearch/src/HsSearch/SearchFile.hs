module HsSearch.SearchFile
  ( SearchFile(..)
  , blankSearchFile
  , isArchiveFile
  , isSearchableFile
  ) where

import HsSearch.FileTypes

-- TODO: use this type with all file-based functions
data SearchFile = SearchFile {
                                searchFileContainers :: [FilePath]
                              , searchFilePath :: FilePath
                              , searchFileType :: FileType
                              } deriving (Show, Eq)

blankSearchFile :: SearchFile
blankSearchFile = SearchFile {
                               searchFileContainers=[]
                             , searchFilePath=""
                             , searchFileType=Unknown
                             }

isArchiveFile :: SearchFile -> Bool
isArchiveFile sf = searchFileType sf == Archive

isSearchableFile :: SearchFile -> Bool
isSearchableFile sf = isSearchableFileType (searchFileType sf)
