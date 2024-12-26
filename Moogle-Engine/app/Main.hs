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

uploadFolder :: FilePath
uploadFolder = "data"

staticFolder :: FilePath
staticFolder = "static"

main :: IO ()
main = do
    createDirectoryIfMissing True uploadFolder
    openBrowser "http://localhost:3000"
    
    -- Cargar documentos al inicio
    docs <- loadDocuments uploadFolder
    
    scotty 3000 $ do
        middleware $ staticPolicy (addBase staticFolder)

        get "/" $ file $ staticFolder </> "index.html"

        -- Nueva ruta para búsqueda
        get "/search" $ do
            query <- param "q"
            let results = searchDocuments (T.unpack query) docs
            json $ map (\(name, score) -> object ["filename" .= name, "score" .= score]) results

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
