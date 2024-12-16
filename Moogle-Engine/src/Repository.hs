module Repository (createRepository, filesWords, titles, wordsFrequency, docsFrequency, allTexts, filePaths, wordsQuery, wordsIDF) where

import System.Directory (doesFileExist, getDirectoryContents, getCurrentDirectory)
import Data.List (isSuffixOf, union, nub)
import qualified Data.Map as Map
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Control.Monad (forM)
import Data.IORef (newIORef, writeIORef, readIORef, IORef)
import Control.Monad.IO.Class (liftIO)
import System.FilePath (takeBaseName)
import System.IO.Unsafe (unsafePerformIO)
import Document (normalize, tf, idf)

-- Almacenar arrays con los documentos separados por palabras
filesWords :: IORef [[T.Text]]
filesWords = unsafePerformIO $ newIORef []

-- Almacenar los títulos de los documentos
titles :: IORef [T.Text] -- Cambiar a T.Text
titles = unsafePerformIO $ newIORef []

-- Almacenar las frecuencias de palabras en los documentos
wordsFrequency :: IORef [Map.Map T.Text Float]
wordsFrequency = unsafePerformIO $ newIORef []

-- Almacenar la frecuencia de documentos para cada palabra
docsFrequency :: IORef (Map.Map T.Text Float)
docsFrequency = unsafePerformIO $ newIORef Map.empty

-- Almacenar los textos completos de los documentos
allTexts :: IORef [T.Text]
allTexts = unsafePerformIO $ newIORef []

-- Almacenar las rutas de los documentos
filePaths :: IORef [FilePath]
filePaths = unsafePerformIO $ newIORef []

-- Almacenar la consulta de palabras
wordsQuery :: IORef [T.Text]
wordsQuery = unsafePerformIO $ newIORef []

-- Almacenar IDF de las palabras
wordsIDF :: IORef [Float]
wordsIDF = unsafePerformIO $ newIORef []

-- Crear el repositorio y almacenar la información
createRepository :: FilePath -> IO ()
createRepository dir = do
    putStrLn "Leyendo archivos del directorio..."
    currentDir <- getCurrentDirectory
    files <- getDirectoryContents (currentDir ++ "/" ++ dir)
    let txtFiles = filter (\f -> ".txt" `isSuffixOf` f) files
    filePathsVal <- forM txtFiles $ \f -> do
        let path = dir ++ "/" ++ f
        return path
    writeIORef filePaths filePathsVal
    putStrLn $ "Archivos TXT encontrados: " ++ show txtFiles
    
    fileTitles <- forM filePathsVal $ \path -> do
        let title = T.pack $ takeBaseName path -- Convertir a T.Text
        return title
    writeIORef titles fileTitles
    
    texts <- forM filePathsVal $ \path -> do
        text <- TIO.readFile path
        return text
    writeIORef allTexts texts
    
    normalizedWords <- forM texts $ \text -> do
        let normalized = normalize text
        return (T.words normalized)
    writeIORef filesWords normalizedWords
    
    freqList <- forM normalizedWords $ \wordsList -> do
        let freqMap = foldr (\word m -> Map.insertWith (+) word 1 m) Map.empty wordsList
        return freqMap
    writeIORef wordsFrequency freqList
    
    let allWords = concat normalizedWords
    let docFreqMap = foldr (\word m -> Map.insertWith (+) word 1 m) Map.empty (nub allWords)
    writeIORef docsFrequency docFreqMap
    
    totalDocs <- length <$> readIORef filesWords
    updatedFreqList <- forM freqList $ \freqMap -> do
        let updatedMap = Map.map (\f -> tf f (fromIntegral $ Map.size freqMap)) freqMap
        return updatedMap
    writeIORef wordsFrequency updatedFreqList
    
    let idfMap = Map.map (\f -> idf totalDocs f) docFreqMap
    let idfList = Map.elems idfMap
    writeIORef wordsIDF idfList
    let queryKeys = Map.keys docFreqMap
    writeIORef wordsQuery queryKeys
