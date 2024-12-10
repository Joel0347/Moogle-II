{-# LANGUAGE OverloadedStrings #-}

import Happstack.Server (nullConf, simpleHTTP, ok, dir, path, serveDirectory, Browsing(DisableBrowsing), toResponse)
import Data.List (nub)
import Control.Monad (msum)
import Indexer (indexDirectory)
import Searcher (searchKeywords)
import Web.Browser (openBrowser)
import Control.Concurrent (forkIO)

main :: IO ()
main = do
    index <- indexDirectory "data"
    forkIO $ simpleHTTP nullConf $ msum
        [ dir "static" $ serveDirectory DisableBrowsing [] "static"
        , dir "search" $ path $ \query -> do
            let results = nub (map fst (searchKeywords (words query) index))
            ok $ toResponse (unlines results)
        , serveDirectory DisableBrowsing [] "static"
        ]
    _ <- openBrowser "http://localhost:8000/static/index.html"
    putStrLn "Servidor iniciado en http://localhost:8000"
    _ <- getLine
    return ()
