module Lib
    ( searchDocuments
    , loadDocuments
    , Document
    , DocumentCollection
    ) where

import Data.List
import Data.Char
import qualified Data.Map as Map
import System.Directory
import Control.Monad

type Document = (String, String)
type Term = String
type DocumentCollection = [Document]

calculateTF :: Term -> String -> Double
calculateTF term doc = 
    let wordCount = length $ filter (== term) $ words doc
        totalWords = length $ words doc
    in if totalWords == 0 
       then 0 
       else fromIntegral wordCount / fromIntegral totalWords

calculateIDF :: Term -> DocumentCollection -> Double
calculateIDF term docs = 
    let totalDocs = fromIntegral $ length docs
        docsWithTerm = fromIntegral $ length $ filter (containsTerm term) docs
    in if docsWithTerm == 0 
       then 0 
       else log (totalDocs / docsWithTerm)
  where
    containsTerm t (_, content) = t `elem` words content

calculateTFIDF :: Term -> Document -> DocumentCollection -> Double
calculateTFIDF term (_, content) docs = 
    let tf = calculateTF term content
        idf = calculateIDF term docs
    in tf * idf

searchDocuments :: String -> DocumentCollection -> [(String, Double)]
searchDocuments query docs = 
    let queryTerms = words $ normalizeText query
        scores = [(docName, sum [calculateTFIDF term (docName, content) docs | term <- queryTerms])
                | (docName, content) <- docs]
    in reverse $ sortOn snd $ filter ((> 0) . snd) scores

normalizeText :: String -> String
normalizeText = map toLower . filter (not . isPunctuation)

loadDocuments :: FilePath -> IO DocumentCollection
loadDocuments dir = do
    files <- listDirectory dir
    contents <- forM files $ \file -> do
        content <- readFile (dir ++ "/" ++ file)
        return (file, normalizeText content)
    return contents
