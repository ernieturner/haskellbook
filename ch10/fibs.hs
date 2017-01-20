module Fibs where

fibs = 1 : scanl (+) 1 fibs

fibsN x = fibs !! x

fibs20 = take 20 fibs

fibsL100 = takeWhile (< 100) fibs

myFactorial = scanl (*) 1 [1..]

myFactorial20 = take 20 myFactorial