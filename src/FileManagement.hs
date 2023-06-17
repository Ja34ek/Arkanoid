module FileManagement where

import System.IO
import Data.List
import System.IO
import Data.List
import System.Directory
import System.FilePath
import Text.Read (readMaybe)
import Data.Maybe (mapMaybe)

createTextFile :: FilePath -> IO ()
createTextFile path = do
  createDirectoryIfMissing True (takeDirectory path)
  handle <- openFile path WriteMode
  hClose handle

updateHighScores :: FilePath -> Int -> IO ()
updateHighScores path newScore = do
  let tempPath = path ++ ".tmp"
  createTextFile tempPath
  scores <- readHighScores path
  
  let updatedScores = insert newScore scores
      sortedScores = sortOn (negate . snd) (zip [1..] updatedScores)

  writeHighScores tempPath sortedScores
  renameFile tempPath path

readHighScores :: FilePath -> IO [Int]
readHighScores path = do
  contents <- readFile path
  let scores = mapMaybe parseScore (lines contents)
  return scores

parseScore :: String -> Maybe Int
parseScore str = case words str of
  (_:scoreStr:_) -> readMaybe scoreStr
  _ -> Nothing

writeHighScores :: FilePath -> [(Int, Int)] -> IO ()
writeHighScores path scores = do
  let formattedScores = map formatScore scores
      content = unlines formattedScores
  writeFile path content

formatScore :: (Int, Int) -> String
formatScore (rank, score) = show rank ++ ". " ++ show score