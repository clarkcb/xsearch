{-# LANGUAGE Arrows, NoMonomorphismRestriction #-}
module HsSearch.FileTypes
  ( FileType(..)
  , getFileTypes
  , isSearchableFileType
  ) where

import Text.XML.HXT.Core

import HsSearch.FileUtility (getExtension, normalizeExtension)
import HsSearch.Paths_hssearch (getDataFileName)

data FileType = Archive
              | Binary
              | Text
              | Unknown
  deriving (Show, Eq)

data XmlFileType = XmlFileType { name :: String, extensions :: [String] }
  deriving (Show, Eq)

fileTypesXmlFile :: FilePath
fileTypesXmlFile = "filetypes.xml"

atTag tag = deep (isElem >>> hasName tag)

text = getChildren >>> getText

getXmlFileType = atTag "filetype" >>>
  proc f -> do
    ftname <- getAttrValue "name" -< f
    exts <- text <<< atTag "extensions" -< f
    returnA -< XmlFileType { name = ftname, extensions = map normalizeExtension $ words exts }

getXmlFileTypes :: IO [XmlFileType]
getXmlFileTypes = do
  fileTypesXmlPath <- getDataFileName fileTypesXmlFile
  runX (readDocument [withValidate no] fileTypesXmlPath >>> getXmlFileType)

getFileTypes :: [FilePath] -> IO [FileType]
getFileTypes files = do
  xmlFileTypes <- getXmlFileTypes
  return $ map (getFileType xmlFileTypes) files

getFileType :: [XmlFileType] -> FilePath -> FileType
getFileType xmlFileTypes f =
  case getExtension f of
    Just x -> matchingTypeForExtension xmlFileTypes x
    Nothing -> Unknown

isSearchableFileType :: FileType -> Bool
isSearchableFileType t = t `elem` [Text, Binary, Archive]

matchingTypeForExtension :: [XmlFileType] -> String -> FileType
matchingTypeForExtension xmlFileTypes x =
  case filter (\f -> x `elem` (extensions f)) xmlFileTypes of
    [] -> Unknown
    fts -> case fileTypeName fts of
           "archive" -> Archive
           "binary" -> Binary
           tname | tname `elem` ["code", "text", "xml"] -> Text
           _ -> Unknown
  where fileTypeName = (name . head)
