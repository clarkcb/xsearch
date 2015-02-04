module Main (main) where

import HsSearch.FileUtilTest

import Test.Framework
import Test.Framework.Providers.HUnit
import Test.HUnit hiding (Test)

main :: IO ()
main = do
  fileUtilTests <- getFileUtilTests
  defaultMain fileUtilTests
