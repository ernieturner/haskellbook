module ChapterEx where

tensDigit :: Integral a => a -> a
tensDigit x = d
    where xLast = x `div` 10
          d     = xLast `mod` 10

-- divMod

tensDigit2 :: Integral a => a -> a
tensDigit2 x = snd d
    where d = divMod 10 x


foldBool :: a -> a -> Bool -> a
foldBool x y True = x
foldBool x y False = y

foldBoolCase :: a -> a -> Bool -> a
foldBoolCase x y z
    case where z of
        True -> x
        False -> y

foldBoolGuard :: a -> a -> Bool -> a
foldBoolGuard x y z
    | z == True  = x