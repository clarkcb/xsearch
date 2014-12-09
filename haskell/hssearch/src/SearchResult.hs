module SearchResult
  ( SearchResult(..)
  , blankSearchResult
  , formatSearchResult
  ) where

import qualified Data.ByteString as BL
import Language.Haskell.TH.Ppr

data SearchResult = SearchResult {
                                   searchPattern :: String
                                 , filePath :: FilePath
                                 , lineNum :: Int
                                 , matchStartIndex :: Int
                                 , matchEndIndex :: Int
                                 , line :: BL.ByteString
                                 } deriving (Show, Eq)

blankSearchResult :: SearchResult
blankSearchResult = SearchResult {
                                   searchPattern=""
                                 , filePath=""
                                 , lineNum=0
                                 , matchStartIndex=0
                                 , matchEndIndex=0
                                 , line=BL.empty
                                 }

formatSearchResult :: SearchResult -> String
formatSearchResult result = filePath result ++ ": " ++
                            show (lineNum result) ++ ": [" ++
                            show (matchStartIndex result) ++ ":" ++
                            show (matchEndIndex result) ++ "]: " ++
                            trimLeadingWhitespace (bytesToString $ BL.unpack (line result))

trimLeadingWhitespace :: String -> String
trimLeadingWhitespace s = dropWhile isWhitespace s
  where isWhitespace c = c `elem` [' ', '\t']
