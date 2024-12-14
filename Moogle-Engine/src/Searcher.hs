module Searcher (searchKeywords) where

import Data.List (isInfixOf)
import qualified Data.Map as Map
import qualified Data.Text as T

searchKeywords :: [String] -> [(FilePath, String)] -> [(FilePath, String)]
searchKeywords keywords index = filter (\(file, content) -> any (`isInfixOf` content) keywords) index

termFrequency :: T.Text -> Map.Map T.Text Int -> Double
termFrequency word wordCount =
    let count = Map.findWithDefault 0 word wordCount
        total = sum $ Map.elems wordCount
    in fromIntegral count / fromIntegral total

inverseDocumentFrequency :: T.Text -> Map.Map T.Text Int -> Int -> Double
inverseDocumentFrequency word globalWordCount totalDocs =
    let docCount = Map.findWithDefault 0 word globalWordCount
    in log (fromIntegral totalDocs / fromIntegral (docCount + 1))
