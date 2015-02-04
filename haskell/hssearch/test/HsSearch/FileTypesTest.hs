module HsSearch.FileTypesTest (getFileTypeTests) where

import HsSearch.FileTypes

import Test.Framework
import Test.Framework.Providers.HUnit
import Test.HUnit hiding (Test)

getFileTypeTests :: IO [Test]
getFileTypeTests = do
  let archiveFile = "archive.zip"
  let binaryFile = "binary.exe"
  let textFile = "text.txt"
  let unknownFile = "unknown.xyz"
  archiveFileType <- getFileType archiveFile
  binaryFileType <- getFileType binaryFile
  textFileType <- getFileType textFile
  unknownFileType <- getFileType unknownFile
  return [ testCase "getFileType archive.zip == Archive" (archiveFileType @?= Archive)
         , testCase "getFileType binary.exe == Binary" (binaryFileType @?= Binary)
         , testCase "getFileType text.txt == Text" (textFileType @?= Text)
         , testCase "getFileType unknown.xyz == Unknown" (unknownFileType @?= Unknown)
         ]
