module RankingVector (getScore, wordsQuery, wordsIDF) where

import qualified Data.Map as Map
import qualified Data.Text as T
import Data.IORef (readIORef)
import Repository (wordsFrequency, titles)

-- Almacenar las palabras de la consulta
wordsQuery :: [T.Text]
wordsQuery = []

-- Almacenar los IDF de cada palabra de la consulta
wordsIDF :: [Float]
wordsIDF = []

-- Obtener los scores de los documentos
getScore :: [T.Text] -> [Float] -> [Double] -> IO [Float]
getScore query vectorIDF closenessValues = do
    wordsFrequencyVals <- readIORef wordsFrequency
    titlesVals <- readIORef titles
    let numDocs = length titlesVals
        numQueryWords = length query

    -- Array que devolverá los scores
    let finalVector = replicate numDocs 0

    -- Iteramos sobre la cantidad de documentos
    let calculateScores i finalVec
          | i >= numDocs = return finalVec
          | otherwise = do
              -- Iteramos sobre la cantidad de palabras de la consulta
              let suma = foldl (\acc j -> if Map.member (query !! j) (wordsFrequencyVals !! i)
                                           then acc + (Map.findWithDefault 0 (query !! j) (wordsFrequencyVals !! i)) * vectorIDF !! j
                                           else acc - 10 * fromIntegral numQueryWords) 0 [0..(numQueryWords - 1)]
              let finalVec' = take i finalVec ++ [suma / realToFrac (closenessValues !! i)] ++ drop (i + 1) finalVec
              calculateScores (i + 1) finalVec'
    
    calculateScores 0 finalVector
