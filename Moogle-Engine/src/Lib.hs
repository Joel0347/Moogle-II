module Lib
    (
      loadDocuments
    , jaccardSimilarity
    , Document
    , DocumentCollection
    ) where

import Data.List as DL
import Data.Char
import qualified Data.Map as Map
import System.Directory
import Control.Monad
import Data.Text (Text, words, unpack)
import qualified Data.Set as Set

type Document = (String, String)
type Term = String
type DocumentCollection = [Document]

normalizeText :: String -> String
normalizeText = map toLower . filter (not . isPunctuation)

loadDocuments :: FilePath -> IO DocumentCollection
loadDocuments dir = do
    files <- listDirectory dir
    contents <- forM files $ \file -> do
        content <- readFile (dir ++ "/" ++ file)
        return (file, normalizeText content)
    return contents

jaccardSimilarity :: Document -> Document -> Double
jaccardSimilarity (_, content1) (_, content2) =
    let set1 = Set.fromList $ DL.words content1
        set2 = Set.fromList $ DL.words content2
        intersectionSize = Set.size $ Set.intersection set1 set2
        unionSize = Set.size $ Set.union set1 set2
    in fromIntegral intersectionSize / fromIntegral unionSize

