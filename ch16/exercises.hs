module Exercises where

-- Be Kind
-- 1. *
-- 2. b: * -> *
--    T: *
-- 3. * -> *

replaceWithP :: b -> Char
replaceWithP = const 'p'

lms :: [Maybe [Char]]
lms = [Just "Ave", Nothing, Just "woohoo"]
-- Just making the argument more specific
replaceWithP' :: [Maybe [Char]] -> Char
replaceWithP' = replaceWithP

liftedReplace :: Functor f => f a -> f Char
liftedReplace = fmap replaceWithP

liftedReplace' :: [Maybe [Char]] -> [Char]
liftedReplace' = liftedReplace

twiceLifted :: (Functor f1, Functor f) =>f (f1 a) -> f (f1 Char)
twiceLifted = (fmap . fmap) replaceWithP
-- Making it more specific

twiceLifted' :: [Maybe [Char]] -> [Maybe Char]
twiceLifted' = twiceLifted

thriceLifted :: (Functor f2, Functor f1, Functor f) => f (f1 (f2 a)) -> f (f1 (f2 Char))
thriceLifted = (fmap . fmap . fmap) replaceWithP

-- More specific or "concrete"
thriceLifted' :: [Maybe [Char]] -> [Maybe [Char]]
thriceLifted' = thriceLifted



-- Heavy Lifting
--1.
a = fmap (+1) (read "[1]" :: [Int])

--2.
b = (fmap . fmap) (++ "lol") (Just ["Hi,", "Hello"])

--3.
c = fmap (*2) (\x -> x - 2)

--4
d = fmap ((return '1' ++) . show) (\x -> [x, 1..3])

--5
e :: IO Integer
e = let ioi = readIO "1" :: IO Integer
        ioString :: IO String
        ioString = fmap show ioi
        readAppend :: String -> Integer
        readAppend = (read . ("123"++))
        changed = fmap readAppend ioString
        in fmap (*3) changed

e' :: IO Integer
e' = let ioi = readIO "1" :: IO Integer
         changed = fmap (read . ("123"++) . show) ioi
         in fmap (*3) changed


fe = readIO "\"stuff\"" :: IO String

ee :: IO [[Integer]]
ee = let ioi = readIO "[1]" :: IO [Integer]
         changed :: IO [Integer]
         changed = fmap (read . (\x ->"[1,2,3,"++ (drop 1 x)) . show) ioi
    in (fmap . fmap) (flip (:) [9]) changed


-- Instances of Func
--1.
newtype Identity a = Identity a
instance Functor Identity where
    fmap f (Identity a) = Identity (f a)

--2.
data Pair a = Pair a a
instance Functor Pair where
    fmap f (Pair a b) = Pair (f a) (f b)

--3.
data Two a b = Two a b
instance Functor (Two a) where
    fmap f (Two a b) = Two a (f b)

--4.
data Three a b c = Three a b c
instance Functor (Three a b) where
    fmap f (Three a b c) = Three a b (f c)

--5.
data Three' a b = Three' a b b
instance Functor (Three' a) where
    fmap f (Three' a b c) = Three' a (f b) (f c)

-- 6.
data Four a b c d = Four a b c d
instance Functor (Four a b c) where
    fmap f (Four a b c d) = Four a b c (f d)

-- 7.
data Four' a b = Four' a a a b
instance Functor (Four' a) where
    fmap f (Four' a b c d) = Four' a b c (f d)

-- 8. Cannot, nothing to map over? or something?

liftedInc :: (Functor f, Num b) => f b -> f b
liftedInc = fmap (+1)

-- Possibly
data Possibly a = LolNope | Yeppers a deriving (Eq, Show)
instance Functor Possibly where
    fmap _ LolNope = LolNope
    fmap f (Yeppers a) = Yeppers (f a)

-- Short Exercises

data Sum a b = First a | Second b deriving (Eq, Show)
instance Functor (Sum a) where
    fmap _ (First a) = First a
    fmap f (Second b) = Second (f b)

-- 2. Not quite sure, something about being unable to change the structure

getInt :: IO Int
getInt = fmap read getLine



