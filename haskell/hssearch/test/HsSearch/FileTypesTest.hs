module HsSearch.FileTypesTest
  (
    getFileTypeTests
  , getFileTypeFromNameTests
  ) where

import HsSearch.FileTypes

import Test.Framework
import Test.Framework.Providers.HUnit
import Test.HUnit hiding (Test)

getFileTypeTests :: IO [Test]
getFileTypeTests = do
  let archiveFile = "archive.zip"
  let binaryFile = "binary.exe"
  let codeFile = "FileTypes.hs"
  let textFile = "text.txt"
  let xmlFile = "markup.xml"
  let unknownFile = "unknown.xyz"
  archiveFileType <- getFileType archiveFile
  binaryFileType <- getFileType binaryFile
  codeFileType <- getFileType codeFile
  textFileType <- getFileType textFile
  xmlFileType <- getFileType xmlFile
  unknownFileType <- getFileType unknownFile
  return [ testCase "getFileType archive.zip == Archive" (archiveFileType @?= Archive)
         , testCase "getFileType binary.exe == Binary" (binaryFileType @?= Binary)
         , testCase "getFileType FileTypes.hs == Code" (codeFileType @?= Code)
         , testCase "getFileType text.txt == Text" (textFileType @?= Text)
         , testCase "getFileType markup.xml == Xml" (xmlFileType @?= Xml)
         , testCase "getFileType unknown.xyz == Unknown" (unknownFileType @?= Unknown)
         ]

getFileTypeFromNameTests :: IO [Test]
getFileTypeFromNameTests = do
  let archiveFileType = getFileTypeForName "archive"
  let binaryFileType = getFileTypeForName "binarY"
  let codeFileType = getFileTypeForName "cODe"
  let textFileType = getFileTypeForName "Text"
  let xmlFileType = getFileTypeForName "XML"
  let unknownFileType = getFileTypeForName "whoknows"
  return [ testCase "getFileTypeForName archive == Archive" (archiveFileType @?= Archive)
         , testCase "getFileTypeForName binarY == Binary" (binaryFileType @?= Binary)
         , testCase "getFileTypeForName cODe == Code" (codeFileType @?= Code)
         , testCase "getFileTypeForName Text == Text" (textFileType @?= Text)
         , testCase "getFileTypeForName XML == Xml" (xmlFileType @?= Xml)
         , testCase "getFileTypeForName whoknows == Unknown" (unknownFileType @?= Unknown)
         ]
