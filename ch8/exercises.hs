module Exercises where

applyTimes :: (Eq a, Num a) => a->(b->b)->b->b
applyTimes 0 f b = b
applyTimes n f b = f . applyTimes (n-1) f $ b

-- applyTimes 5 (+1) 5
--
-- 1 + applyTimes (4) (+1) 5
-- 1 + applyTimes (3) (+1) 5
-- 1 + applyTimes (2) (+1) 5
-- 1 + applyTimes (1) (+1) 5
-- 1 + applyTimes (0) (+1) 5
-- 5
-- 5 + 4 + 3 + 2 + 1
-- 10

f :: Bool -> Int
f True = error "blah"
f False = 0

f' :: Bool -> Int
f' False = 0

fibonacci :: Integral a => a -> a
fibonacci 0 = 0
fibonacci 1 = 1
fibonacci x = fibonacci (x - 1) + fibonacci (x - 2)

dividedBy :: Integral a => a -> a -> (a, a)
dividedBy num denom = go num denom 0
    where go n d count
            | n < d = (count, n)
            | otherwise = go (n - d) d (count + 1)


-- 1. d
-- 2. b
-- 3. d
-- 4. b

-- Reviewing Currying

cattyConny :: String -> String -> String
cattyConny x y = x ++ " mrow " ++ y

flippy :: String -> String -> String
flippy = flip cattyConny

appendCatty :: String -> String
appendCatty = cattyConny "woops"

frappe :: String -> String
frappe = flippy "haha"

additive :: Integral a => a -> a
additive 1 = 1
additive x = additive (x - 1) + x


manualMultiple :: Integral a => a -> a -> a
manualMultiple num times = go num (times - 1)
    where go n d
            | d == 0 = n
            | otherwise = go (n + num) (d - 1)
