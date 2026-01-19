module HsSearch.ByteStringUtil
  ( --compareByteStrings
  --, padByteString
    sliceByteString
  , trimLeftByteString
  , trimRightByteString
  ) where

import Data.Char (isSpace, toLower)

import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BC


-- compareByteStrings :: Bool -> String -> String -> Ordering
-- compareByteStrings caseInsensitive s1 s2 =
--   if caseInsensitive
--   then compare (lower s1) (lower s2)
--   else compare s1 s2
--   where lower = map toLower

-- padByteString :: String -> Int -> String
-- padByteString s len | length s < len = s ++ replicate (len - length s) ' '
--                 | otherwise      = s

-- sliceByteString startIdx endIdx s
-- Extracts a substring from 'startIdx' (inclusive) to 'endIdx' (exclusive) indices.
sliceByteString :: Int -> Int -> B.ByteString -> B.ByteString
sliceByteString startIdx endIdx bs = B.take (endIdx - startIdx) (B.drop startIdx bs)

-- subByteString :: Int -> Int -> B.ByteString -> B.ByteString
-- subByteString si ei bs = B.take (ei - si) (B.drop si bs)

-- sliceBS :: Int -> Int -> B.ByteString -> B.ByteString
-- sliceBS startIdx endIdx bs = B.take (endIdx - startIdx) (B.drop startIdx bs)

trimLeftByteString :: B.ByteString -> B.ByteString
trimLeftByteString = BC.dropWhile isSpace

trimRightByteString :: B.ByteString -> B.ByteString
trimRightByteString = B.reverse . trimLeftByteString . B.reverse
