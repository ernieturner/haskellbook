import Data.Char

cap :: [Char] -> [Char]
cap xs = map toUpper xs

rev :: [Char] -> [Char]
rev xs = reverse xs

composed :: [Char] -> [Char]
composed = cap . rev

fmapped :: [Char] -> [Char]
fmapped = fmap cap rev

tupled :: [Char] -> ([Char], [Char])
tupled x = (cap x, rev x)

tupledAp :: [Char] -> ([Char], [Char])
tupledAp = (,) <$> cap <*> rev

tupledMonad :: [Char] -> ([Char], [Char])
tupledMonad = do
    a <- cap
    b <- rev
    return (a, b)

tupledBind :: [Char] -> ([Char], [Char])
tupledBind = cap >>= (\capResult -> rev >>= (\revResult -> return (capResult, revResult)))