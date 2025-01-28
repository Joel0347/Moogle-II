module Lib
    (
      loadDocuments
    , cosineSimilarity
    , Document
    , DocumentCollection
    , buildDictionary
    , suggestCorrection
    , levenshtein
    , tfIdf
    , precomputeTfIdf
    , normalize
    ) where

import Data.List as DL
import Data.Char
import qualified Data.Map as Map
import System.Directory
import Control.Monad
import Data.Text (Text, words, unpack, pack)
import qualified Data.Set as Set
import Data.Array
import Data.Set (Set)
import Data.List (nub, sort, group)
import Data.Map (Map)
import qualified Data.Map as Map
import Control.Parallel.Strategies (parMap, rpar)

type Document = (String, String)
type DocumentCollection = [Document]

-- Normaliza el texto eliminando puntuación y convirtiendo a minúsculas
normalize :: String -> String
normalize = map toLower . filter (`notElem` ",.!?\"'")

-- Calcula la frecuencia de términos (TF) en un documento
termFrequency :: String -> Map String Int
termFrequency docContent = Map.fromListWith (+) [(word, 1) | word <- DL.words (normalize docContent)]

-- Calcula las puntuaciones TF-IDF para un documento
tfIdf :: DocumentCollection -> Map String Int -> Int -> Document -> Map String Double
tfIdf corpus wordCounts numDocs doc =
    let tf = termFrequency (snd doc)
        totalWordsInDoc = sum (Map.elems tf)
        idf = Map.mapWithKey (\word docFreq -> log (fromIntegral numDocs / fromIntegral (docFreq + 1))) wordCounts
    in Map.mapWithKey (\word nd -> (fromIntegral nd / fromIntegral totalWordsInDoc) * Map.findWithDefault 0 word idf) tf

-- Función para precomputar TF-IDF para todas las palabras en cada documento
precomputeTfIdf :: DocumentCollection -> Map String (Map String Double)
precomputeTfIdf docs =
    let numDocs = length docs
        wordCounts = foldr (\(_, content) acc -> foldr (\word -> Map.insertWith (+) word 1) acc (DL.words $ normalize content)) Map.empty docs
        tfIdfMaps = parMap rpar (tfIdf docs wordCounts numDocs) docs
        allWords = Set.toList $ buildDictionary docs
        tfIdfForWord word = Map.fromList [(file, Map.findWithDefault 0 word tfIdfMap) | ((file, _), tfIdfMap) <- zip docs tfIdfMaps]
    in Map.fromList [(word, tfIdfForWord word) | word <- allWords]

-- Calcula la similitud entre dos documentos usando TF-IDF
cosineSimilarity :: Map String Double -> Map String Double -> Double
cosineSimilarity tfIdf1 tfIdf2 =
    let dotProduct = sum [w1 * w2 | (term, w1) <- Map.toList tfIdf1, let w2 = Map.findWithDefault 0 term tfIdf2]
        magnitude m = sqrt $ sum [w^2 | w <- Map.elems m]
        mag1 = magnitude tfIdf1
        mag2 = magnitude tfIdf2
    in dotProduct / (mag1 * mag2)

loadDocuments :: FilePath -> IO DocumentCollection
loadDocuments dir = do
    files <- listDirectory dir
    forM files $ \file -> do
        content <- readFile (dir ++ "/" ++ file)
        return (file, normalize content)

buildDictionary :: DocumentCollection -> Set.Set String
buildDictionary documents = 
    let allWords = concatMap (DL.words . snd) documents
    in Set.fromList allWords

-- Función para calcular la distancia de Levenshtein entre dos cadenas
levenshtein :: String -> String -> Int
levenshtein s1 s2 = levMemo (length s1) (length s2)
  where
    arr = array ((0, 0), (length s1, length s2))
          [((i, j), lev i j) | i <- [0 .. length s1], j <- [0 .. length s2]]

    lev 0 j = j
    lev i 0 = i
    lev i j = minimum [ arr ! (i-1, j) + 1
                      , arr ! (i, j-1) + 1
                      , arr ! (i-1, j-1) + (if s1 !! (i-1) == s2 !! (j-1) then 0 else 1)
                      ]

    levMemo i j = arr ! (i, j)

-- Función ajustada para sugerir correcciones
suggestCorrection :: Set String -> Text -> [(Text, Int)]
suggestCorrection dictionary word =
    let dictionaryList = map pack (Set.toList dictionary)
    in map (\w -> (w, levenshtein (unpack w) (unpack word))) dictionaryList
