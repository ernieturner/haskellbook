module Exercises where

import Data.Monoid
import Data.Foldable hiding (sum)
import Prelude hiding (sum)

-- Library Exercises

sum :: (Foldable t, Num a) => t a -> a
sum = foldr (\a x -> a + x) 0

product :: (Foldable t, Num a) => t a -> a
product = foldr (\a x -> a * x) 1

--elem :: (Foldable t, Eq a) => a -> t a -> Bool
--elem = foldr (\a x -> )

--minimum :: (Foldable t, Ord a) => t a -> Maybe a
--minimum = foldr (\a x -> if x < a then x else a) Nothing

--maximum :: (Foldable t, Ord a) => t a -> Maybe a

--null :: (Foldable t) => t a -> Bool

length :: (Foldable t) => t a -> Int
length = foldr (\_ a -> a+1) 0

--toList :: (Foldable t) => t a -> [a]


--Chapter Exercises

data Constant a b = Constant b

instance Foldable (Constant a) where
    foldr f z (Constant a) = f a z

data Two a b = Two a b

instance Foldable (Two b) where
    foldr f z (Two a b) = f b z

data Three a b c = Three a b c
instance Foldable (Three b c) where
    foldr f z (Three a b c) = f c z

