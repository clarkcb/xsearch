module HsSearch.FileUtilTest (getFileUtilTests) where

import HsSearch.FileUtil

import Test.Framework
import Test.Framework.Providers.HUnit
import Test.HUnit hiding (Test)

getFileUtilTests :: IO [Test]
getFileUtilTests =
  return [ testCase "getExtension file.txt" (getExtension "file.txt" @?= Just ".txt")
         , testCase "getExtension file." (getExtension "file." @?= Nothing)
         , testCase "getExtension file" (getExtension "file" @?= Nothing)
         , testCase "getExtension .file.txt" (getExtension ".file.txt" @?= Just ".txt")
         , testCase "getExtension .file." (getExtension ".file." @?= Nothing)
         , testCase "getExtension .file" (getExtension ".file" @?= Nothing)

         , testCase "normalizeExtension .txt" (normalizeExtension ".txt" @?= ".txt")
         , testCase "normalizeExtension txt" (normalizeExtension "txt" @?= ".txt")
         , testCase "normalizeExtension .TXT" (normalizeExtension ".TXT" @?= ".txt")
         , testCase "normalizeExtension TXT" (normalizeExtension "TXT" @?= ".txt")

         , testCase "hasExtension file.txt .txt" (hasExtension "file.txt" ".txt" @?= True)
         , testCase "hasExtension file.txt .zip" (hasExtension "file.txt" ".zip" @?= False)

         , testCase "isDotDir ." (isDotDir "." @?= True)
         , testCase "isDotDir .." (isDotDir ".." @?= True)
         , testCase "isDotDir .git" (isDotDir ".git" @?= False)

         , testCase "isHiddenFilePath file.txt" (isHiddenFilePath "file.txt" @?= False)
         , testCase "isHiddenFilePath .file.txt" (isHiddenFilePath ".file.txt" @?= True)
         , testCase "isHiddenFilePath .git" (isHiddenFilePath ".git" @?= True)
         ]
