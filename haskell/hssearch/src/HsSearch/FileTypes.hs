{-# LANGUAGE Arrows, NoMonomorphismRestriction #-}
module HsSearch.FileTypes
  ( FileType(..)
  , getFileType
  , getFileTypes
  , getFileTypeForName
  , isSearchableFileType
  ) where

import Data.Char (toLower)
import Text.XML.HXT.Core

import HsSearch.FileUtil (getExtension, normalizeExtension)
import HsSearch.Paths_hssearch (getDataFileName)

data FileType = Unknown
              | Archive
              | Binary
              | Code
              | Text
              | Xml
  deriving (Show, Eq)

searchableFileTypes :: [FileType]
searchableFileTypes = [Text, Binary, Archive]

isSearchableFileType :: FileType -> Bool
isSearchableFileType t = t `elem` searchableFileTypes

data XmlFileType = XmlFileType { name :: String, extensions :: [String] }
  deriving (Show, Eq)

fileTypesXmlFile :: FilePath
fileTypesXmlFile = "filetypes.xml"

atTag :: ArrowXml a => String -> a XmlTree XmlTree
atTag tag = deep (isElem >>> hasName tag)

getXmlFileType :: IOSLA (XIOState ()) XmlTree XmlFileType
getXmlFileType = atTag "filetype" >>>
  proc f -> do
    ftname <- getAttrValue "name" -< f
    exts <- (getChildren >>> getText) <<< atTag "extensions" -< f
    returnA -< XmlFileType { name = ftname, extensions = normalized exts }
  where normalized exts = map normalizeExtension $ words exts

getXmlFileTypes :: IO [XmlFileType]
getXmlFileTypes = do
  fileTypesXmlPath <- getDataFileName fileTypesXmlFile
  runX (readDocument [withValidate no] fileTypesXmlPath >>> getXmlFileType)

getFileType :: FilePath -> IO FileType
getFileType f = do
  fileTypes <- getFileTypes [f]
  case fileTypes of
    [] -> return Unknown
    _  -> return $ head fileTypes

getFileTypes :: [FilePath] -> IO [FileType]
getFileTypes files = do
  xmlFileTypes <- getXmlFileTypes
  return $ map (fileTypeFromXmlFileTypes xmlFileTypes) files

getFileTypeForName :: String -> FileType
getFileTypeForName typeName =
  case (lower typeName) of
    "archive" -> Archive
    "binary" -> Binary
    "code" -> Code
    "xml" -> Xml
    "text" -> Text
    _ -> Unknown
  where lower = map toLower

fileTypeFromXmlFileTypes :: [XmlFileType] -> FilePath -> FileType
fileTypeFromXmlFileTypes xmlFileTypes f =
  case getExtension f of
    Just x -> matchingTypeForExtension xmlFileTypes x
    Nothing -> Unknown

matchingTypeForExtension :: [XmlFileType] -> String -> FileType
matchingTypeForExtension xmlFileTypes x =
  case filter (\f -> x `elem` extensions f) xmlFileTypes of
    [] -> Unknown
    fts -> case fileTypeName fts of
           "archive" -> Archive
           "binary" -> Binary
           "code" -> Code
           "xml" -> Xml
           tname | tname `elem` ["code", "text", "xml"] -> Text
           _ -> Unknown
  where fileTypeName = name . head
