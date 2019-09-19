{-# LANGUAGE OverloadedStrings #-}

module Text.Fractions where

import Control.Applicative
import Data.Ratio ((%))
import Text.Trifecta

badFraction = "1/0"
alsoBad = "10"
shouldWork = "1/2"
shouldAlsoWork = "2/1"


parseFraction :: Parser Rational
parseFraction = do
    numerator <- decimal
    char '/'
    denominator <- decimal
    return (numerator % denominator)

virtuousFraction :: Parser Rational
virtuousFraction = do
    numerator <- decimal
    char '/'
    denominator <- decimal
    case denominator of
        0 -> fail "Denominator cannot be zero"
        _ -> return (numerator % denominator)

parseReturnNumber :: Parser Integer
parseReturnNumber = do
  number <- decimal
  eof
  return number

main :: IO ()
main = do
    print $ parseString parseFraction mempty shouldWork
    print $ parseString parseFraction mempty shouldAlsoWork
    print $ parseString parseFraction mempty alsoBad
    print $ parseString parseFraction mempty badFraction

main2 :: IO ()
main2 = do
    print $ parseString virtuousFraction mempty alsoBad
    print $ parseString virtuousFraction mempty badFraction
    print $ parseString virtuousFraction mempty shouldWork
    print $ parseString virtuousFraction mempty shouldAlsoWork

main3 :: IO ()
main3 = do
  print $ parseString parseReturnNumber mempty "123"
  print $ parseString parseReturnNumber mempty "123abc"