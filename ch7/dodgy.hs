module Dodgy where

dodgy x y = x + y * 10
oneIsOne = dodgy 1
oneIsTwo = (flip dodgy) 2

pal x
    | x == reverse x = True
    | otherwise        = False

numbers x
    | x < 0  = -1
    | x == 0 = 0
    | x > 0  = 1