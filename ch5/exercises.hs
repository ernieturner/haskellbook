-- Type Matching
-- a => c
-- b => d
-- c => b
-- d => a
-- e => e

-- Type Arguments
-- 1. a
-- 2. d
-- 3. d?
-- 4. c
-- 5. ?
-- 6. b
-- 7. e
-- 8. e?
-- 9. c

-- Parametricity
-- 1. FU
-- 2. f x y = y
--    f y x = x
-- 3. f x y = y

-- Apply Yourself
-- 1. String a => a -> a
-- 2. Num a => a -> a (Fractional?)
-- 3. Int -> [Char]
-- 4. Ord a -> Bool  (Int a -> Bool)
-- 5. Char a -> Bool

-- Chapter Exercises
-- 1. c
-- 2. a
-- 3. b
-- 4. d. Actually c


-- Determine the Type
-- 1.
--   a) Num
--   b) (Num a, String b)
--   c) (Int a, String b)
--   d) Bool
--   e) Int
--   f) Bool
-- 2. Num
-- 3. Num a -> a
-- 4. Fractional
-- 5. String

-- Does it Compile
-- 1. bigNum isn't a function, it's a value
-- 2. GTG
-- 3. b 10 doesn't work because b isn't a function, should instead be c = a b 10
-- 4. c is not defined

-- Type variable or specific type?
-- 1.
-- 2. zed - Fully Polymorphic
--    Zed - Concrete
--    Blah - Concrete
-- 3. a -> Fully polymorphinc
--    b -> Constrained polymorphic
--    C -> Concrete
-- 4. f -> Fully Polymorphic
--    g -> Fully Polymorphic
--    C -> Concrete

-- Write a type signature
-- 1. functionH :: [x] -> x?
-- 2. functionC :: (Ord x, Ord y) => x -> y -> Bool  - Actually (Ord x) => x -> x -> Bool
-- 3. functionS :: x -> y -> y

-- Write the function
-- 1. i x = x
-- 2. f a b -> a
-- 3. Yes
-- 4. f a b -> b
-- 5. ?
-- 6. ?
-- 7. ?
-- 8. ?

-- Type Kwon Do
