-- Mood Swing
-- 1. Mood
-- 2. Blah/Woot
-- 3. Function could return both Woot or Blah
-- 4.
    -- changeMood Mood = Woot
    -- changeMood ??
-- 5. ??

-- Find the Mistakes
-- 1. not True && True
-- 2. not (x == 6)
-- 3. No problem
-- 4. ["Merry"] > ["Happy"]
-- 5. Both not strings, unsure what the goal is

-- Chapter Exercises
-- 1. [a] -> Int
-- 2.
    -- a: 5
    -- b: 3
    -- c: 2
    -- d: 5
-- 3. 6 / length [1, 2, 3] fails, but not entirely sure why...
-- 4. Also don't know why 6 / (length [1, 2, 3])
-- 5. True
-- 6. False
-- 7.
    -- a: True
    -- b: Error, not all of same type in list
    -- c: 5
    -- d: False
    -- e: Error, not all Bool
-- 8.
let isPalindrome x = reverse x == x

-- 9.
myAbs :: Integer -> Integer
myAbs x =
    if x > 0
        then x
    else
        then x * (-1)

-- 10
combineShit :: (a, b) -> (c, d) -> ((b, d), (a, c))
combineShit x y = ((snd x, fst y), (fst x, snd y))


-- Correcting Syntax
-- 1.
F xs = x w 1
    where w = length xs

-- 2.
\ X = X

-- 3.
\ x : xs -> x?

-- 4.
f (a b) = a

-- Type Matching
-- 1. a
-- 2. b
-- 3. a
-- 4. d
