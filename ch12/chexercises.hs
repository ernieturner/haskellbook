module Chexercises where

type Name = String
type Age = Integer

data Person = Person Name Age deriving Show

data PersonInvalid = NameEmpty
                   | AgeTooLow
                   deriving (Eq, Show)

mkPerson :: Name -> Age -> Either PersonInvalid Person
mkPerson name age
    | name == "" = Left NameEmpty
    | age < 0 = Left AgeTooLow
    | otherwise = Right $ Person name age