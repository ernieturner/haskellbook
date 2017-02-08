module Game
    (freshPuzzle, runGame)
    where

import Control.Monad (forever)
import Data.Maybe (isJust)
import System.Exit (exitSuccess)
import Data.Char (toLower)

import PuzzleType

freshPuzzle :: String -> Puzzle
freshPuzzle word = Puzzle word list []
    where list = map (const Nothing) word


charInWord :: Puzzle -> Char -> Bool
charInWord (Puzzle word _ _) guess
  | elem guess word = True
  | otherwise = False


alreadyGuessed :: Puzzle -> Char -> Bool
alreadyGuessed (Puzzle _ _ list) guess
  | elem guess list = True
  | otherwise = False


fillInCharacter :: Puzzle -> Char -> Puzzle
fillInCharacter (Puzzle word filledInSoFar s) c =
  Puzzle word newFilledInSoFar (c : s)
    where zipper guessed wordChar guessChar =
            if wordChar == guessed
            then Just wordChar
            else guessChar
          newFilledInSoFar =
            zipWith (zipper c) word filledInSoFar


handleGuess :: Puzzle -> Char -> IO Puzzle
handleGuess puzzle guess = do
    putStrLn $ "Your guess was: " ++ [guess]
    case (charInWord puzzle guess
        , alreadyGuessed puzzle guess) of
        (_, True) -> do
            putStrLn "You already guessed that character, pick something else!"
            return puzzle
        (True, _) -> do
            putStrLn "This character was in the word, filling in the word accordingly"
            return (fillInCharacter puzzle guess)
        (False, _) -> do
            putStrLn "This character wasn't in the word, try again."
            return (fillInCharacter puzzle guess)

incorrectGuesses :: String -> String -> Int
incorrectGuesses word guesses = wrongWordCount
    where
        wrongWordCount = foldr doesExist 0 guesses
        doesExist = \a b ->
            if elem a word == True
            then b
            else b + 1


gameOver :: Puzzle -> IO ()
gameOver (Puzzle wordToGuess _ guessed) =
    if (incorrectGuesses wordToGuess guessed) > 7 then
        do putStrLn "You lose!"
           putStrLn $ "The word was: " ++ wordToGuess
           exitSuccess
    else return ()


gameWin :: Puzzle -> IO ()
gameWin (Puzzle word filledInSoFar _) =
    if all isJust filledInSoFar
    then
        do putStrLn ("You win! You guessed the word: " ++ word)
           exitSuccess
    else return ()


runGame :: Puzzle -> IO ()
runGame puzzle = forever $ do
    gameWin puzzle
    gameOver puzzle
    putStrLn $ "Current puzzle is: " ++ show puzzle
    putStr "Guess a letter: "
    guess <- getLine
    case guess of
        [c] -> handleGuess puzzle (toLower c) >>= runGame
        _ ->putStrLn "Your guess must be a single character"
