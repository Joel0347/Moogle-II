{-# LANGUAGE OverloadedStrings #-}

module Main where

import Web.Scotty
import System.Directory (listDirectory, doesFileExist, createDirectoryIfMissing)
import System.FilePath ((</>))
import Network.Wai.Middleware.Static
import Control.Monad.IO.Class (liftIO)
import Network.HTTP.Types.Status (status404)
import Web.Browser (openBrowser)
import Network.HTTP.Types.Header (hContentDisposition)
import qualified Data.Text.Lazy as T
import qualified Data.Text as TS
import qualified Data.ByteString.Lazy as BL
import Network.Wai.Parse (defaultParseRequestBodyOptions, parseRequestBody, lbsBackEnd, FileInfo(..))

-- Configuración del servidor
uploadFolder :: FilePath
uploadFolder = "data"

staticFolder :: FilePath
staticFolder = "static"

main :: IO ()
main = do
    -- Asegurarnos de que la carpeta de subida existe
    createDirectoryIfMissing True uploadFolder

    -- Abrir automáticamente el navegador
    openBrowser "http://localhost:3000"  -- Abre el navegador con la URL del servidor

    -- Iniciar el servidor
    scotty 3000 $ do
        -- Servir archivos estáticos desde 'staticFolder'
        middleware $ staticPolicy (addBase staticFolder)

        -- Ruta principal: servir el index.html
        get "/" $ do
            file $ staticFolder </> "index.html"

        -- Listar archivos disponibles en la carpeta de subida
        get "/files" $ do
            files <- liftIO $ listDirectory uploadFolder
            json files

        -- Descargar un archivo
        get "/download/:filename" $ do
            filename <- param "filename"
            let filePath = uploadFolder </> T.unpack filename
            fileExists <- liftIO $ doesFileExist filePath
            if fileExists
                then do
                    setHeader "Content-Disposition" ("attachment; filename=" `T.append` filename)
                    setHeader "Content-Type" "application/octet-stream"
                    file filePath
                else status status404 >> text "Archivo no encontrado."

        -- Servir el archivo JavaScript
        get "/static/js/script.js" $ do
            file $ staticFolder </> "js" </> "script.js"

        -- Servir el archivo CSS
        get "/static/css/style.css" $ do
            file $ staticFolder </> "css" </> "style.css"
            
        -- Asegurarse de servir archivos estáticos desde la carpeta 'uploadFolder' (data)
        middleware $ staticPolicy (addBase uploadFolder)
