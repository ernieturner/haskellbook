module Exercises where

import Data.Char

upperOnly :: String -> String
upperOnly = filter (\x -> isUpper x)

capitalize :: String -> String
capitalize x = (toUpper (head x)) : tail x

capAll :: String -> String
capAll "" = ""
capAll (x:xs) = toUpper x : capAll xs

firstCapOnly :: String -> Char
firstCapOnly x = toUpper $ head x

-- firstCapOnly x = toUpper . head x
-- firstCapOnly = toUpper . head


myOr :: [Bool] -> Bool
myOr [] = False
myOr (x:xs) = x == True || myOr xs


myAny :: (a -> Bool) -> [a] -> Bool
myAny _ [] = False
myAny f (x:xs) = (f x) == True || myAny f xs


myElem :: Eq a => a -> [a] -> Bool
myElem _ [] = False
myElem item (x:xs) = item == x || myElem item xs


myElemWithAny :: Eq a => a -> [a] -> Bool
myElemWithAny item xs = any ( == item) xs


myReverse :: [a] -> [a]
myReverse [] = []
myReverse (x:xs) = (myReverse xs) ++ [x]


squish :: [[a]] -> [a]
squish [] = []
squish (x:xs) = x ++ squish xs


squishMap :: (a -> [b]) -> [a] -> [b]
squishMap _ [] = []
squishMap f (x:xs) = f x ++ squishMap f xs


squishAgain :: [[a]] -> [a]
squishAgain x = (squishMap id) x


