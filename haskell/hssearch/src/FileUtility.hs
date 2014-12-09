{-# LANGUAGE ScopedTypeVariables #-}

module FileUtility
    (
      filterDirectories
    , filterFiles
    , getDirectoryFiles
    , getExtension
    , getFileByteString
    , getFileLines
    , getNonDotDirectoryContents
    , getRecursiveContents
    , getRecursiveDirectories
    , hasExtension
    , isHiddenFilePath
    , normalizeExtension
    ) where

import Control.Exception (IOException, bracket, handle)
import Control.Monad (filterM, forM)
import qualified Data.ByteString as BL
import qualified Data.ByteString.Internal as BL (c2w)
import Data.Char (toLower)
import Data.List (elemIndices, isPrefixOf)
import System.Directory (doesDirectoryExist, doesFileExist, getDirectoryContents)
import System.FilePath ((</>), splitPath, takeFileName)
import System.IO (hClose, hSetNewlineMode, IOMode(..), universalNewlineMode, openFile)

-- preferring this over System.FilePath (takeExtension)
getExtension :: String -> Maybe String
getExtension "" = Nothing
getExtension fp = case takeFileName fp of
                  name | "." `isPrefixOf` name -> Nothing
                  name | '.' `elem` name -> Just $ trimToExt name
                  _ -> Nothing
  where trimToExt name = drop (maximum (elemIndices '.' name)) name

normalizeExtension :: String -> String
normalizeExtension x = case x of
                       ('.':_) -> lower x
                       _ -> ['.'] ++ lower x
  where lower s = map toLower s

hasExtension :: String -> String -> Bool
hasExtension fp x = getExtension fp == (Just . normalizeExtension) x

filterDirectories :: [FilePath] -> IO [FilePath]
filterDirectories = filterM doesDirectoryExist

filterFiles :: [FilePath] -> IO [FilePath]
filterFiles = filterM doesFileExist

isHiddenFilePath :: FilePath -> Bool
isHiddenFilePath f = any isHidden pathElems
  where isHidden = isPrefixOf "."
        pathElems = filter (`notElem` ["./", "."]) $ splitPath f

getDirectoryFiles :: FilePath -> IO [FilePath]
getDirectoryFiles dir = do
  names <- getNonDotDirectoryContents dir
  files <- filterFiles names
  return files

getNonDotDirectoryContents :: FilePath -> IO [FilePath]
getNonDotDirectoryContents dir = do
  names <- getDirectoryContents dir
  let filteredNames = filter notDotDir names
  return $ map (dir </>) filteredNames

notDotDir :: FilePath -> Bool
notDotDir dir = dir `notElem` [".", ".."]

getRecursiveContents :: FilePath -> IO [FilePath]
getRecursiveContents dir = do
  filePaths <- getNonDotDirectoryContents dir
  paths <- forM filePaths $ \path -> do
    isDirectory <- doesDirectoryExist path
    if isDirectory
      then do
        subNames <- getRecursiveContents path
        return $ [path] ++ subNames
    else return [path]
  return (concat paths)

getRecursiveDirectories :: FilePath -> IO [FilePath]
getRecursiveDirectories dir = do
  filePaths <- getNonDotDirectoryContents dir
  paths <- forM filePaths $ \path -> do
    isDirectory <- doesDirectoryExist path
    if isDirectory
      then do
        subNames <- getRecursiveDirectories path
        return $ [path] ++ subNames
    else return []
  return (concat paths)

getFileByteString :: FilePath -> IO (Either String BL.ByteString)
getFileByteString f = handle (\(e :: IOException) -> return (Left (show e))) $
  bracket (openFile f ReadMode) hClose $ \h -> do
    hSetNewlineMode h universalNewlineMode
    contents <- BL.hGetContents h
    return (Right contents)

getFileLines :: FilePath -> IO (Either String [BL.ByteString])
getFileLines f = do
  fileByteString <- getFileByteString f
  case fileByteString of
    Left e -> return $ Left e
    Right c -> return $ Right (BL.split (BL.c2w '\n') c)
