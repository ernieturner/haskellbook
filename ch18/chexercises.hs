module Chexercises where

import Control.Monad (join)
-- join :: Monad m => m (m a) -> m a

bind :: Monad m => (a -> m b) -> m a -> m b
bind f a = join (fmap f a)

twiceWhenEven :: [Integer] -> [Integer]
twiceWhenEven xs = do
    x <- xs
    if even x
        then [x * x, x * x]
        else [x * x]

twiceWhenEven' :: [Integer] -> [Integer]
twiceWhenEven' xs = do
    x <-xs
    if even x
        then [x, x * x]
        else []

data Cow = Cow {
    name ::String
  , age :: Int
  , weight :: Int
 } deriving (Eq, Show)

noEmpty :: String -> Maybe String
noEmpty "" = Nothing
noEmpty str = Just str

noNegative :: Int -> Maybe Int
noNegative n
    | n >= 0 = Just n
    | otherwise = Nothing

weightCheck :: Cow -> Maybe Cow
weightCheck c =
    if n == "Bess" && w > 499
        then Nothing
        else Just c
    where w = weight c
          n = name c

mkSphericalCow :: String -> Int -> Int -> Maybe Cow
mkSphericalCow name' age' weight' =
    case noEmpty name' of
        Nothing -> Nothing
        Just nammy ->
            case noNegative age' of
                Nothing -> Nothing
                Just agey ->
                    case noNegative weight' of
                        Nothing -> Nothing
                        Just weighty ->
                            weightCheck (Cow nammy agey weighty)

mkSphericalCow' :: String -> Int -> Int -> Maybe Cow
mkSphericalCow' name' age' weight' = do
    nammy <- noEmpty name'
    agey <- noNegative age'
    weighty <- noNegative weight'
    weightCheck (Cow nammy agey weighty)

mkSphericalCow'' :: String -> Int -> Int -> Maybe Cow
mkSphericalCow'' name' age' weight' =
    noEmpty name' >>=
    \ nammy ->
        noNegative age' >>=
        \ agey ->
            noNegative weight' >>=
            \ weighty ->
                weightCheck (Cow nammy agey weighty)

data Sum a b = First a | Second b deriving (Eq, Show)

-- Either Monad

instance Functor (Sum a) where
    fmap _ (First a) = First a
    fmap f (Second b) = Second (f b)

instance Applicative (Sum a) where
    pure a = Second a
    _ <*> (First a) = First a
    (First a) <*> _ = First a
    (Second f) <*> (Second b) = Second (f b)

instance Monad (Sum a) where
    return = pure
    (First a) >>= _ = First a
    (Second a) >>= f = f a

data CountMe a = CountMe Integer a deriving (Eq, Show)

instance Functor CountMe where
    fmap f (CountMe i a) = CountMe i (f a)

instance Applicative CountMe where
    pure = CountMe 0
    (CountMe i f) <*> (CountMe i' a) = CountMe (i + i') (f a)

instance Monad CountMe where
    return = pure
    (CountMe _ a) >>= f = (f a)

