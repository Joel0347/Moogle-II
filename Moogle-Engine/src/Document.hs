module Document (normalize, normalizeQuery, tf, idf, getSnippet) where

import Data.Char (toLower)
import Data.List (intercalate, maximumBy, elemIndices)
import Data.Ord (comparing)
import qualified Data.Text as T

-- Normalizar el texto: convertir a minúsculas, eliminar puntuación, etc.
normalize :: T.Text -> T.Text
normalize = T.toLower . T.map replaceSpecialChars
  where
    replaceSpecialChars c
      | c `elem` "áéíóú" = toEnum (fromEnum c - 1)  -- Reemplazar caracteres con acentos.
      | c `elem` "ñ" = 'ñ'
      | c `elem` ['a'..'z'] ++ ['0'..'9'] = c
      | otherwise = ' '

-- Normalizar la consulta de búsqueda
normalizeQuery :: T.Text -> T.Text
normalizeQuery = T.toLower . T.map replaceSpecialChars
  where
    replaceSpecialChars c
      | c `elem` "áéíóú" = toEnum (fromEnum c - 1)  -- Reemplazar caracteres con acentos.
      | c `elem` "ñ!*~" = c
      | c `elem` ['a'..'z'] ++ ['0'..'9'] = c
      | otherwise = ' '

-- Calcular el TF de las palabras
tf :: Float -> Float -> Float
tf wordFrequency totalWords = wordFrequency / totalWords

-- Calcular el IDF de las palabras
idf :: Int -> Float -> Float
idf totalDocs docFrequency = log (fromIntegral totalDocs / docFrequency)

-- Obtener el snippet de los textos que resultaron de la búsqueda
getSnippet :: [T.Text] -> [Float] -> Int -> [T.Text] -> T.Text
getSnippet allTexts wordIdfs docIndex wordsQuery = do
    let text = normalize (allTexts !! docIndex)

        findBestWord idxs = maximumBy (comparing (wordIdfs !!)) idxs
        wordIndexes = concatMap (\kw -> elemIndices kw (T.words text)) wordsQuery
        bestWordIndex = findBestWord wordIndexes

        snippetStart = max 0 (bestWordIndex - 150)
        snippetEnd = min (T.length text) (bestWordIndex + 150)
    
    T.take (snippetEnd - snippetStart) (T.drop snippetStart text)
