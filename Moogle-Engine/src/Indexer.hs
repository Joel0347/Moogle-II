module Indexer (indexDirectory) where

import System.Directory (doesFileExist, getDirectoryContents)
import Data.List (isSuffixOf)
import qualified Data.Map as Map
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Control.Monad (forM)

type DocumentName = String
type DocumentText = T.Text
type WordCount = Map.Map T.Text Int
type DocumentWords = Map.Map DocumentName [T.Text]
type DocumentWordCount = Map.Map DocumentName WordCount
type GlobalWordCount = Map.Map T.Text Int

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
