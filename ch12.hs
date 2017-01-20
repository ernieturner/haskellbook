module Exercises where

import Data.Maybe

newtype Word' = Word' String deriving (Eq, Show)

vowels = "aeiou"

isVowel :: Char -> Bool
isVowel x = elem x vowels

vowelCount :: String -> Integer
vowelCount word = foldr countVowels 0 word
    where countVowels item acc
            | isVowel item = (acc + 1)
            | otherwise = acc

lengthWithoutSpaces :: String -> Integer
lengthWithoutSpaces word = foldr countChars 0 word
    where countChars item acc
            | item == ' ' = acc
            | otherwise = (acc + 1)

mkWord :: String -> Maybe Word'
mkWord word
        | totalVowels > consonantCount = Nothing
        | otherwise = Just (Word' word)
        where
            totalVowels = vowelCount word
            consonantCount = (lengthWithoutSpaces word) - totalVowels




data Nat = Zero | Succ Nat deriving (Eq, Show)

natToInteger :: Nat -> Integer
natToInteger Zero = 0
natToInteger (Succ x) = 1 + natToInteger x

integerToNat :: Integer -> Maybe Nat
integerToNat number
    | number < 0 = Nothing
    | otherwise = Just (rep number)
        where rep num
                | num == 0 = Zero
                | otherwise = Succ (rep (num - 1))



--1.
myIsJust :: Maybe a -> Bool
myIsJust Nothing = False
myIsJust _ = True

myIsNothing :: Maybe a -> Bool
myIsNothing Nothing = True
myIsNothing _ = False


--2.
mayybee :: b -> (a -> b) -> Maybe a -> b
mayybee b _ Nothing = b
mayybee _ func (Just n) = func n

--fromMaybe :: a -> Maybe a -> a
--fromMaybe _ Nothing = 0
--fromMaybe (Just n) = n

listToMaybe :: [a] -> Maybe a
listToMaybe [] = Nothing
listToMaybe (x:_) = Just x

maybeToList :: Maybe a -> [a]
maybeToList Nothing = []
maybeToList (Just a) = [a]

getJustVal :: Maybe a -> a
getJustVal (Just a) = a

myCatMaybes :: [Maybe a] -> [a]
myCatMaybes list = map getJustVal (filter (\x -> myIsJust x) list)

flipMaybes :: [Maybe a] -> Maybe [a]
flipMaybes (Nothing:xs) = Just (moreItems xs)
flipMaybes (Just a:xs) = Just ([a] ++ moreItems xs)
    where moreItems list
            | (x:xs) = (getJustVal x) ++ (moreItems xs)
            | _ = []