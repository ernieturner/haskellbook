module Exercises where

import Data.Char

shiftChar :: Char -> Int -> Char
shiftChar x shiftLength
    | m > endChar = chr (startChar + (m - endChar - 1))
    | m < startChar = chr (endChar - (startChar - m - 1))
    | otherwise = chr m
    where
        startChar = ord 'a' --97
        endChar = ord 'z'   --122
        m = (ord x) + shiftLength

caesar :: String -> String
caesar "" = ""
caesar (x:xs) = shiftChar x 3 : caesar xs

uncaesar :: String -> String
uncaesar "" = ""
uncaesar (x:xs) = shiftChar x (-3) : uncaesar xs