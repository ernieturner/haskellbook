module Exercises where

stops  = "pbtdkg"
vowels = "aeiou"



seekritFunc x =
     (/) (fromIntegral (sum (map length (words x))))
           (fromIntegral (length (words x)))

-- 2. Calculates the average length of words in a string
-- 3.


-- Fold Functions

myOr :: [Bool] -> Bool
myOr = foldr (||) False

myAny :: (a -> Bool) -> [a] -> Bool
myAny f xs = foldr (\a b -> ((f a) || b)) False xs

--PointFree
myAnyPF :: (a -> Bool) -> [a] -> Bool
myAnyPF f = foldr (\a b -> (f a) || b) False

--Better?
myAnyPFPlus :: (a -> Bool) -> [a] -> Bool
myAnyPFPlus f = foldr (\a b -> b || (f a)) False


myElem :: Eq a => a -> [a] -> Bool
myElem x = foldr (\a b -> b || x == a) False

myReverse :: [a] -> [a]
myReverse = foldr (\a b -> b ++ [a]) []

myMap :: (a -> b) -> [a] -> [b]
myMap f = foldr (\a b -> b ++ [f a]) []