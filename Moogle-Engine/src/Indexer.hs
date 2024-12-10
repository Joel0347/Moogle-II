module Indexer (indexDirectory) where

import System.Directory (doesFileExist, getDirectoryContents)
import Data.List (isSuffixOf)

indexDirectory :: FilePath -> IO [(FilePath, String)]
indexDirectory dir = do
    files <- getDirectoryContents dir
    let txtFiles = filter (\f -> ".txt" `isSuffixOf` f) files
    fileContents <- mapM readFileIfExists (map (\f -> dir ++ "/" ++ f) txtFiles)
    return $ zip txtFiles fileContents

readFileIfExists :: FilePath -> IO (String)
readFileIfExists file = do
    exists <- doesFileExist file
    if exists
        then readFile file
        else do
            putStrLn $ "El archivo no existe: " ++ file
            return ""
