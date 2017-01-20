module Exercises where

import Data.Char

--1. a
--2. c
--3. b
--4. c

-- As Patterns

beginsWith :: (Eq a) => [a] -> [a] -> Bool
beginsWith [] _ = True
beginsWith _ [] = False
beginsWith (x:xs) (y:ys) = x == y && beginsWith xs ys

isSubsequenceOf :: (Eq a) => [a] -> [a] -> Bool
isSubsequenceOf [] [] = True
isSubsequenceOf [] ys = True
isSubsequenceOf _ [] = False

isSubsequenceOf ax@(x:xs) (y:ys)
    | x == y = isSubsequenceOf xs ys
    | otherwise = isSubsequenceOf ax ys


capitalizeWords :: String -> [(String, String)]
capitalizeWords xs = map f (words xs)
    where f as@(s:st) = (as, toUpper s : st)

capitalizeWord :: String -> String
capitalizeWord (w:ws) = toUpper w : ws

