module WordPicker
  (randomWord)
  where

import System.Random (randomRIO)
import Data.Char (toLower)

minWordLength :: Int
minWordLength = 4

maxWordLength :: Int
maxWordLength = 12

type WordList = [String]
allWords :: IO WordList
allWords = do
    dict <- readFile "data/dict.txt"
    return (lines dict)


gameWords :: IO WordList
gameWords = do
    aw <- allWords
    return (filter gameLength aw)
    where gameLength w =
            let l = length (w :: String)
            in l > minWordLength && l < maxWordLength


randomWord' :: WordList -> IO String
randomWord' wl = do
    randomIndex <- randomRIO (0 , (length wl) - 1)
    return $ map toLower (wl !! randomIndex)

randomWord :: IO String
randomWord = gameWords >>= randomWord'