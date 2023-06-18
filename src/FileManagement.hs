module FileManagement where

import System.IO
import System.Directory
import System.FilePath
import Data.List
import Data.Time
import Data.Ord
import Data.Time.Format
import Data.List
import Data.Maybe
import Text.Read
import Control.Monad

createTextFile :: FilePath -> IO ()
createTextFile path = do
  removeIfExists path
  createDirectoryIfMissing True (takeDirectory path)
  withFile path WriteMode $ \handle ->
    hClose handle

compareAndUpdateLines :: FilePath -> FilePath -> IO ()
compareAndUpdateLines path path2 = do
  exists <- doesFileExist path2
  if exists
    then do
      sessionRecord <- readFirstLineAsInt path
      personalRecord <- readFirstLineAsInt path2
      let updatedLine = max sessionRecord personalRecord
      writeFirstLine path2 updatedLine
    else do
      createTextFile path2
      firstLine <- readFirstLineAsInt path
      writeFirstLine path2 firstLine

readFirstLineAsInt :: FilePath -> IO Int
readFirstLineAsInt path = do
  withFile path ReadMode $ \handle -> do
    contents <- hGetLine handle
    case readMaybe contents of
      Just value -> return value
      Nothing -> error "Invalid format in file"

writeFirstLine :: FilePath -> Int -> IO ()
writeFirstLine path value = withFile path AppendMode $ \handle -> do
  hPrint handle value
  currentTime <- getCurrentTime
  let formattedDateTime = formatTime defaultTimeLocale "%Y-%m-%d %H:%M:%S" currentTime
  hPutStrLn handle formattedDateTime

removeIfExists :: FilePath -> IO ()
removeIfExists path = do
  exists <- doesFileExist path
  when exists $ removeFile path

updateHighScores :: FilePath -> Int -> IO ()
updateHighScores path newScore = do
  let tempPath = path ++ ".tmp"
  createTextFile tempPath
  scores <- readHighScores path

  let updatedScores = insert newScore scores
      sortedScores = sortOn Down updatedScores

  writeHighScores tempPath sortedScores
  renameFile tempPath path

readHighScores :: FilePath -> IO [Int]
readHighScores path = do
  contents <- readFile path
  let scores = mapMaybe parseScore (lines contents)
  return scores

parseScore :: String -> Maybe Int
parseScore str = readMaybe str

writeHighScores :: FilePath -> [Int] -> IO ()
writeHighScores path scores = do
  let formattedScores = map show scores
      content = unlines formattedScores
  writeFile path content