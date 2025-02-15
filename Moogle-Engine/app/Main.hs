{-# LANGUAGE OverloadedStrings #-}

module Main where

import Lib
import Web.Scotty
import System.Directory
import System.FilePath
import Network.Wai.Middleware.Static
import Control.Monad.IO.Class
import Network.HTTP.Types.Status
import Web.Browser
import qualified Data.Text.Lazy as TL
import qualified Data.Text as T
import Data.Aeson (object, (.=))
import Data.List (sortBy, words, isInfixOf)  -- Agregado isInfixOf
import Data.Ord (comparing)
import qualified Data.Set as Set
import qualified Data.Map as Map
import Data.Map (Map)
import Control.Parallel.Strategies (parMap, rpar)
import System.IO
import qualified Data.Text.IO as TIO
import Data.Text.Encoding (decodeUtf8, decodeUtf8')
import qualified Data.ByteString as BS
import Data.Char (chr)
import System.FilePath

uploadFolder :: FilePath
uploadFolder = "data"

cacheFile :: FilePath
cacheFile = "tfidf_cache.txt"

staticFolder :: FilePath
staticFolder = "static"

-- Precomputar y guardar TF-IDF en un archivo de texto
saveTfIdfCache :: FilePath -> Map String (Map String Double) -> IO ()
saveTfIdfCache filename tfIdfDict = do
    let serialized = show tfIdfDict
    writeFile filename serialized

-- Cargar TF-IDF desde un archivo de texto
loadTfIdfCache :: FilePath -> IO (Map String (Map String Double))
loadTfIdfCache filename = do
    exists <- doesFileExist filename
    if exists
        then do
            content <- readFile filename
            return (read content)
        else return Map.empty

-- Crear un diccionario de frecuencias de palabras para la consulta
queryWordFreq :: String -> Map String Int
queryWordFreq query =
    Map.fromListWith (+) [(word, 1) | word <- words $ normalize query]

-- Buscar palabras de la consulta en el diccionario precomputado y sumar los valores de TF-IDF ponderados por su frecuencia en la consulta
queryTfIdfSum :: Map String (Map String Double) -> Map String Int -> Map String Double
queryTfIdfSum tfIdfDict queryFreq =
    let wordTfIdf word = Map.findWithDefault Map.empty word tfIdfDict
        docTfIdfSums = Map.foldrWithKey (\word freq acc -> Map.unionWith (+) (Map.map (* fromIntegral freq) (wordTfIdf word)) acc) Map.empty queryFreq
    in docTfIdfSums

getDocumentSnippet :: String -> String -> Int -> String  -- Cambiado el tipo de maxLen a Int
getDocumentSnippet content query maxLen = 
    let words = filter (not . null) $ lines content
        queryWords = map normalize $ Data.List.words query
        containsQuery line = any (\qw -> qw `isInfixOf` normalize line) queryWords
        relevant = filter containsQuery words
        snippet = if null relevant 
                 then take maxLen content  -- Si no hay coincidencias, tomar el inicio
                 else take maxLen $ head relevant  -- Tomar el primer fragmento relevante
    in snippet

main :: IO ()
main = do
    -- Cargar documentos al inicio
    docs <- loadDocuments uploadFolder

    -- Cargar TF-IDF desde la caché o recalcular si es necesario
    tfIdfDictCache <- loadTfIdfCache cacheFile
    tfIdfDict <- if Map.null tfIdfDictCache
                 then do
                     let newTfIdfDict = precomputeTfIdf docs
                     saveTfIdfCache cacheFile newTfIdfDict
                     putStrLn "TF-IDF cache generated and saved."
                     return newTfIdfDict
                 else do
                     putStrLn "TF-IDF cache loaded."
                     return tfIdfDictCache

    let dictionary = buildDictionary(docs)
    
    createDirectoryIfMissing True uploadFolder
    openBrowser "http://localhost:3000"
    
    scotty 3000 $ do
        -- Middleware para servir archivos estáticos
        middleware $ staticPolicy (addBase staticFolder)

        -- Ruta principal
        get "/" $ file $ staticFolder </> "index.html"

        -- Ruta para búsqueda
        get "/search" $ do
            query <- param "query"
            let strictQuery = TL.toStrict query
            let queryFreq = queryWordFreq (T.unpack strictQuery)
            let tfIdfSums = queryTfIdfSum tfIdfDict queryFreq
            let results = Map.toList tfIdfSums
            let filteredResults = filter (\(_, sim) -> sim /= 0) results
            let sortedResults = sortBy (comparing (negate . snd)) filteredResults
            if null sortedResults
                then do
                    let suggestions = suggestQueryCorrection dictionary query
                    json $ map (\(suggestion, dist) -> object ["suggestion" .= suggestion, "distance" .= dist]) suggestions
                else do
                    json $ map (\(doc, sim) -> object ["document" .= doc, "similarity" .= sim]) sortedResults

        -- Ruta para listar archivos
        get "/files" $ do
            files <- liftIO $ listDirectory uploadFolder
            json files

        -- Ruta para descargar archivos
        get "/download/:filename" $ do
            filename <- param "filename"
            let filePath = uploadFolder </> TL.unpack filename
            liftIO $ putStrLn $ "Attempting to download file: " ++ filePath
            fileExists <- liftIO $ doesFileExist filePath
            liftIO $ putStrLn $ "File exists: " ++ show fileExists
            if fileExists
                then do
                    liftIO $ putStrLn "Setting headers for file download..."
                    setHeader "Content-Disposition" ("attachment; filename=" `TL.append` filename)
                    setHeader "Content-Type" "application/octet-stream"
                    file filePath
                else do
                    liftIO $ putStrLn "File not found."
                    status status404
                    text "Archivo no encontrado."

        -- Ruta para obtener snippets
        get "/snippet/:filename" $ do
            filename <- param "filename"
            query <- param "query"
            let filePath = uploadFolder </> TL.unpack filename
            fileExists <- liftIO $ doesFileExist filePath
            if fileExists
                then do
                    content <- liftIO $ case takeExtension filePath of
                        ".docx" -> readDocx filePath
                        _       -> readFile filePath
                    let snippet = getDocumentSnippet content (TL.unpack query) 200
                    json $ object ["snippet" .= snippet]
                else do
                    status status404
                    json $ object ["error" .= ("Archivo no encontrado" :: String)]

        -- Middleware para archivos en el directorio de subida
        middleware $ staticPolicy (addBase uploadFolder)
