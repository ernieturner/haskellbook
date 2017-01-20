module TypeInference1 where

f :: (Num a) => a -> a -> a
f x y = x + y + 3

bobsFavorite :: (Num a) => a -> a -> a
bobsFavorite x y = x + y + z
    where list = [1, 2, 3]
          z = length list