module MidExercises where

-- foldr (:) [] [1..3]
-- (1 :  (2 : (3 : []))) -> [3, 2, 1]

-- foldl (flip (:)) [] [1..3]
-- ((([] : 1) : 2 ) : 3) -- without flip. Bad because [] : 1 is bad
-- ((([] `f` 1) `f` 2) `f` 3) -- with flip. Params are reversed so int comes before [] -> [1, 2, 3]

-- Understanding Folds

-- 1. b
-- 2. foldl (flip (*)) 1 [1..3]
--    ((1 * 1) * 2) * 3) -- without flip
--    1 * (1 * (2 * 3))
-- 3. c
-- 4. a
-- 5.
--    a. foldr (++) "" ["woot", "WOOT", "woot"]
--    b. foldr max [] ["fear", "is", "the", "little", "death"]
--    c. foldr (&&) True [False, True]
--    d. foldr (||) True [False, True] - Will always return True
--    e. foldl ((++) . show) "" [1..5] ????
--    f. foldr (flip const) 'a' [1..5]
--    g. foldr (flip const) 0 "tacos"
--    h. foldl const 0 "burritos"
--    i. foldl const 'z' [1..5]



-- Database Processing

import Data.Time


data DatabaseItem = DbString String
                  | DbNumber Integer
                  | DbDate UTCTime
                    deriving (Eq, Ord, Show)

theDatabase :: [DatabaseItem]
theDatabase =
    [ DbDate (UTCTime
              (fromGregorian 1911 5 1)
      (secondsToDiffTime 34123))
    , DbNumber 9001
    , DbString "Hello, world!"
    , DbDate (UTCTime
                (fromGregorian 1921 5 1)
                (secondsToDiffTime 34123))
    ]

filterDbDate :: [DatabaseItem] -> [UTCTime]
filterDbDate =