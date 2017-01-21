module Exercises where

import Data.Maybe
import Data.List

--1. * -> *
--2. a = * -> *
--   f = *

--notThe :: String -> Maybe String
--notThe "the" = Nothing
--notThe x = Just x

replaceThe' :: String -> String
replaceThe' "the" = "a"
replaceThe' x = x

replaceThe :: String -> String
replaceThe x = intercalate " " (map replaceThe' (words x))


isVowel :: Char -> Bool
isVowel x = (x == 'a') ||
            (x == 'e') ||
            (x == 'i') ||
            (x == 'o') ||
            (x == 'u')

countVowel :: Integer -> [String] -> Integer
countVowel count (x:y:xs)
    | x == "the" && isVowel (head y) = countVowel (count + 1) (y:xs)
    |otherwise = countVowel count (y:xs)
countVowel count _ = count


countTheBeforeVowel :: String -> Integer
countTheBeforeVowel text = countVowel 0 (words text)



vowelCount' :: Integer -> String -> Integer
vowelCount' count (x:xs)
    | isVowel x = vowelCount' (count + 1) xs
    | otherwise = vowelCount' count xs
vowelCount' count _ = count

vowelCount :: String -> Integer
vowelCount = vowelCount' 0

