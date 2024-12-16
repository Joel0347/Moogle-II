module Moogle (query, SearchResult(..), SearchItem(..)) where

import qualified Data.Map as Map
import qualified Data.Text as T
import Data.List (nub, sortBy, union, elemIndex)
import Data.Ord (comparing)
import Control.Monad (forM)
import Control.Monad.IO.Class (liftIO)
import Data.IORef (readIORef)
import Data.Maybe (fromJust)
import Document (normalize, normalizeQuery, tf, idf, getSnippet)
import Repository (filesWords, titles, wordsFrequency, docsFrequency, allTexts, wordsQuery, wordsIDF)
import RankingVector (getScore)

data SearchResult = SearchResult
    { items :: [SearchItem]
    , queryText :: T.Text -- Cambiar el nombre aquí
    } deriving (Show)

data SearchItem = SearchItem
    { title :: T.Text
    , snippet :: T.Text
    , score :: Float
    } deriving (Show)

-- Función principal para manejar la consulta
query :: T.Text -> IO SearchResult
query userQuery = do
    putStrLn $ "Procesando consulta: " ++ T.unpack userQuery

    let justQuery = nub $ T.words $ normalizeQuery userQuery
    let normalizedQuery = normalize userQuery
    let repeatedWords = T.words normalizedQuery
    let wordCount = Map.fromListWith (+) [(word, 1) | word <- repeatedWords]

    wordsQueryVals <- readIORef wordsQuery
    titlesVals <- readIORef titles
    docsFrequencyVals <- readIORef docsFrequency
    wordsFrequencyVals <- readIORef wordsFrequency

    putStrLn $ "Consulta normalizada: " ++ T.unpack normalizedQuery
    putStrLn $ "Palabras de la consulta: " ++ show justQuery

    let idfValues = map (\word -> idf (length titlesVals) (Map.findWithDefault 0 word docsFrequencyVals) * fromIntegral (Map.findWithDefault 1 word wordCount)) justQuery
    let closenessValues = replicate (length titlesVals) 1.0
    scores <- getScore justQuery idfValues closenessValues

    let rankingDocs = foldl (\acc (score, title) -> Map.insertWith (++) score [title] acc) Map.empty (zip scores titlesVals)
    let sortedScores = reverse $ sortBy (comparing fst) (Map.toList rankingDocs)

    putStrLn $ "Scores calculados: " ++ show scores
    putStrLn $ "Documentos clasificados: " ++ show sortedScores

    items <- liftIO $ createSearchItems sortedScores
    putStrLn $ "Resultados de búsqueda: " ++ show items

    return $ SearchResult items normalizedQuery

createSearchItems :: [(Float, [T.Text])] -> IO [SearchItem]
createSearchItems sortedScores = do
    allTextsVals <- readIORef allTexts
    let maxResults = 10
    let scoreItems = take maxResults sortedScores
    items <- forM scoreItems $ \(score, titles) -> do
        mapM (createSearchItem score allTextsVals) titles
    return $ concat items

createSearchItem :: Float -> [T.Text] -> T.Text -> IO SearchItem
createSearchItem score allTextsVals title = do
    titlesVals <- readIORef titles
    let index = fromJust $ elemIndex title titlesVals
    wordsIDFVals <- readIORef wordsIDF
    wordsQueryVals <- readIORef wordsQuery
    let snippet = getSnippet allTextsVals wordsIDFVals index wordsQueryVals
    return $ SearchItem title snippet score
