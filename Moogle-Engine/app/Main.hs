{-# LANGUAGE OverloadedStrings #-}

import Happstack.Server (nullConf, simpleHTTP, ok, dir, serveDirectory, Browsing(DisableBrowsing), toResponse, lookText')
import Data.List (nub, sortBy)
import Control.Monad (msum)
import Repository (createRepository)
import Moogle (query, SearchResult(..), SearchItem(..))
import Web.Browser (openBrowser)
import Control.Concurrent (forkIO)
import qualified Data.Text as T
import Data.Maybe (fromMaybe)
import Text.Read (readMaybe)
import Control.Monad.IO.Class (liftIO)

-- Paginación
pageSize :: Int
pageSize = 10

main :: IO ()
main = do
    putStrLn "Cargando el repositorio..."
    createRepository "data"
    putStrLn "Repositorio cargado."

    _ <- forkIO $ simpleHTTP nullConf $ msum
        [ dir "static" $ serveDirectory DisableBrowsing ["index.html"] "static"
        , dir "search" $ do
            queryParam <- lookText' "q"
            liftIO $ putStrLn $ "Query: " ++ T.unpack queryParam
            searchResult <- liftIO $ query queryParam
            let resultItems = items searchResult
            let paginatedResults = take pageSize resultItems
            liftIO $ putStrLn $ "Paginated Results: " ++ show paginatedResults
            let responseText = T.unpack $ T.unlines $ map formatResult paginatedResults
            liftIO $ putStrLn $ "Response Text: " ++ responseText
            ok $ toResponse responseText
        ]
    _ <- openBrowser "http://localhost:8000/static/index.html"
    putStrLn "Servidor iniciado en http://localhost:8000"
    _ <- getLine
    return ()

formatResult :: SearchItem -> T.Text
formatResult (SearchItem title snippet score) =
    "Title: " <> title <> "\nSnippet: " <> snippet <> "\nScore: " <> T.pack (show score) <> "\n\n"
