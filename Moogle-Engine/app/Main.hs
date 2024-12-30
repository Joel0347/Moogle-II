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
import qualified Data.Text.Lazy as T
import Data.Aeson (object, (.=))
import Data.List (sortBy)
import Data.Ord (comparing)

uploadFolder :: FilePath
uploadFolder = "data"

staticFolder :: FilePath
staticFolder = "static"

main :: IO ()
main = do
    -- Cargar documentos al inicio
    docs <- loadDocuments uploadFolder

    createDirectoryIfMissing True uploadFolder
    openBrowser "http://localhost:3000"
    
    scotty 3000 $ do
        middleware $ staticPolicy (addBase staticFolder)

        get "/" $ file $ staticFolder </> "index.html"

        -- Nueva ruta para búsqueda
        get "/search" $ do
            query <- param "query"
            liftIO $ putStrLn $ "Query: " ++ T.unpack query
            let queryText = ("query", T.unpack query) :: Document
            let results = map (\doc -> (doc, jaccardSimilarity queryText doc)) docs
            let filteredResults = filter (\(_, sim) -> sim > 0.1) results
            let sortedResults = sortBy (comparing (negate . snd)) filteredResults
            json $ map (\(doc, sim) -> object ["document" .= fst doc, "similarity" .= sim]) sortedResults

        -- Rutas existentes para archivos estáticos
        get "/files" $ do
            files <- liftIO $ listDirectory uploadFolder
            json files

        get "/download/:filename" $ do
            filename <- param "filename"
            let filePath = uploadFolder </> T.unpack filename
            fileExists <- liftIO $ doesFileExist filePath
            if fileExists
                then do
                    setHeader "Content-Disposition" ("attachment; filename=" `T.append` filename)
                    file filePath
                else status status404 >> text "Archivo no encontrado."

        middleware $ staticPolicy (addBase uploadFolder)
