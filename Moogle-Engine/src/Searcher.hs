module Searcher (searchKeywords) where

import Data.List (isInfixOf)

searchKeywords :: [String] -> [(FilePath, String)] -> [(FilePath, String)]
searchKeywords keywords index = filter (\(file, content) -> any (`isInfixOf` content) keywords) index
