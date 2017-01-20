module Reverse where

rvrs :: String -> String
rvrs input = y
    where
        firstWord = take 5 input
        secondWord = drop 9 input
        y = concat [secondWord, " is ", firstWord]

main :: IO ()
main = print $ rvrs "Curry is awesome"