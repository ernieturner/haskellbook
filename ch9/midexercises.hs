module Exercises where

myHead :: [a] -> Maybe a
myHead [] = Nothing
myHead (x:xs) = Just x

-- EnumFromTo

eftBool :: Bool -> Bool -> [Bool]
eftBool True False = [True, False]
eftBool False True = [False, True]

--eftOrd :: Ordering -> Ordering -> [Ordering]
--no idea here?

eftInt :: Int -> Int -> [Int]
eftInt from to = go [from] to
    where go list end
            | last list == end = list
            | otherwise = go (list ++ [succ $ last list]) end

eftChar :: Char -> Char -> [Char]
eftChar from to = go [from] to
    where go list end
            | last list == end = list
            | otherwise = go (list ++ [succ $ last list]) end

-- Leifs (better) version
eftInt2 :: Int -> Int -> [Int]
eftInt2 f t
  | f <= t = f : eftInt (succ f) t
  | otherwise = []

-- Thy Fearful Symmetry

myWord :: String -> [String]
myWord [] = []
myWord (' ':xs) = myWord xs
myWord xs = takeWhile (/=' ') xs:myWord (dropWhile (/= ' ') xs)


firstSen = "Tyger Tyger, burning bright\n"
secondSen = "In the forests of the night\n"
thirdSen = "What immortal hand or eye\n"
fourthSen = "Could frame thy fearful symmetry?"
sentences = firstSen ++ secondSen ++ thirdSen ++ fourthSen

myLines :: String -> [String]
myLines [] = []
myLines ('\n':xs) = myLines xs
myLines xs = takeWhile (/='\n') xs:myLines (dropWhile (/= '\n') xs)

--myLinesAndWords :: String -> Char -> [String]
--myLinesAndWords [] sep = []
--myLinesAndWords (sep:xs) sep = myLinesAndWords xs sep
--myLinesAndWords xs sep = takeWhile (/=sep) xs:myLinesAndWords (dropWhile (/=sep) xs)

-- Comprehend Thy Lists

-- mySqr = [1, 4, 9, 16, 25]
mySqr = [x^2 | x <- [1..5]]
--[x | x <-mySqr, rem x 2 == 0]
-- [4, 16]

--[(x,y) | x <- mySqr, y <- mySqr, x < 50, y > 50]
-- [] - y will never be greater than 50

--take 5 [(x,y) | x <- mySqr, y <- mySqr, x < 50, y > 50 ]
-- [] - same as above, y will never be greater than 50

-- Square Cube
myCube = [y^3 | y <- [1..5]]

--allTuples = [(x, y) | x <- mySqr, y <- myCube]
--allSmallTuples = [(x, y) | x <- mySqr, y <- myCube, x < 50, y < 50]
--allSmallTuples length = length allSmallTuples

-- Bottom Madness
-- 1. bottom
-- 2. Value
-- 3. Bottom
-- 4. Value
-- 5. Bottom
-- 6. Value
-- 7. Bottom?
-- 8. Value
-- 9. Value
-- 10. Bottom

-- Normal or WHNF
-- 1. 1
-- 2. 2
-- 3. 1
-- 4. ?
-- 5. ?
-- 6. 1
-- 7. 2

-- More Bottoms
-- 1. Bottom
-- 2. Value
-- 3. Bottom
-- 4. Takes a String and returns a list where every vowel index is true
-- 5.
--  a. Array of 10 items each to a power of 2 [1, 4, 9, 16, 25, etc]
--  b. [1, 10, 20]
--- c. [15, 15, 15]
-- 6.

-- Filtering
filterThree = filter (\x -> mod x 3 == 0) [1..30]
filterThreeLength = length filterThree

filterArticles = filter (\x -> not (elem x ["the", "a", "an"])) . words

-- Zipping Exercises

myZip :: [a] -> [b] -> [(a, b)]
myZip a b
  | length a == 0 || length b == 0 = []
  | otherwise = (head a, head b) : myZip (tail a) (tail b)

myZipWith :: (a -> b -> c) -> [a] -> [b] -> [c]
myZipWith f a b
  | length a == 0 || length b == 0 = []
  | otherwise = f (head a) (head b) : myZipWith f (tail a) (tail b)

zipCombined :: [a] -> [b] -> [(a, b)]
zipCombined listA listB = myZipWith (\a b -> (a, b)) listA listB

