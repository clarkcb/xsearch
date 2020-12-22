{-# LANGUAGE DeriveGeneric, NoMonomorphismRestriction, OverloadedStrings #-}
module HsSearch.FileTypes
  ( FileType(..)
  , JsonFileType(..)
  , getFileType
  , getFileTypes
  , getFileTypeForName
  , getJsonFileTypes
  , isSearchableFileType
  ) where

import qualified Data.ByteString.Lazy.Char8 as BC
import Data.Char (toLower)
import Data.Text(pack, unpack, replace)

import GHC.Generics
import Data.Aeson

import HsSearch.FileUtil (getExtension, normalizeExtension, getFileString)
import HsSearch.Paths_hssearch (getDataFileName)

data FileType = Unknown
              | Archive
              | Binary
              | Code
              | Text
              | Xml
  deriving (Show, Eq)

searchableFileTypes :: [FileType]
searchableFileTypes = [Archive, Binary, Code, Text, Xml]

isSearchableFileType :: FileType -> Bool
isSearchableFileType t = t `elem` searchableFileTypes

getFileTypeForName :: String -> FileType
getFileTypeForName typeName =
  case lower typeName of
    "archive" -> Archive
    "binary" -> Binary
    "code" -> Code
    "xml" -> Xml
    "text" -> Text
    _ -> Unknown
  where lower = map toLower

data JsonFileType = JsonFileType
    { fileType :: String
    , extensions :: [String]
    } deriving (Show, Eq, Generic)

instance FromJSON JsonFileType where
  parseJSON = genericParseJSON defaultOptions {
                fieldLabelModifier = unpack . replace "fileType" "type" . pack
              }

newtype JsonFileTypes
  = JsonFileTypes {filetypes :: [JsonFileType]}
  deriving (Show, Eq, Generic)

instance FromJSON JsonFileTypes

fileTypesJsonFile :: FilePath
fileTypesJsonFile = "filetypes.json"

getJsonFileTypes :: IO [JsonFileType]
getJsonFileTypes = do
  fileTypesJsonPath <- getDataFileName fileTypesJsonFile
  fileTypesJsonString <- getFileString fileTypesJsonPath
  case fileTypesJsonString of
    (Left _) -> return []
    (Right jsonString) ->
      case (eitherDecode (BC.pack jsonString) :: Either String JsonFileTypes) of
        (Left _) -> return []
        (Right jsonFileTypes) -> return (map normalizeType (filetypes jsonFileTypes))
  where normalizeType :: JsonFileType -> JsonFileType
        normalizeType ft = JsonFileType { fileType = fileType ft,
                                          extensions = map normalizeExtension (extensions ft) }

getFileType :: FilePath -> IO FileType
getFileType f = do
  fileTypes <- getFileTypes [f]
  case fileTypes of
    [] -> return Unknown
    _  -> return $ head fileTypes

getFileTypes :: [FilePath] -> IO [FileType]
getFileTypes files = do
  jsonFileTypes <- getJsonFileTypes
  return $ map (fileTypeFromJsonFileTypes jsonFileTypes) files

fileTypeFromJsonFileTypes :: [JsonFileType] -> FilePath -> FileType
fileTypeFromJsonFileTypes jsonFileTypes f =
  case getExtension f of
    Just x -> matchingTypeForExtensionJson jsonFileTypes x
    Nothing -> Unknown

matchingTypeForExtensionJson :: [JsonFileType] -> String -> FileType
matchingTypeForExtensionJson jsonFileTypes x =
  case filter (\f -> x `elem` extensions f) jsonFileTypes of
    [] -> Unknown
    fts -> case fileTypeName fts of
           "archive" -> Archive
           "binary" -> Binary
           "code" -> Code
           "xml" -> Xml
           tname | tname `elem` ["code", "text", "xml"] -> Text
           _ -> Unknown
  where fileTypeName = fileType . head
