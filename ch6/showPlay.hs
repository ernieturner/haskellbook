module ShowPlay where

data Mood =
    One | Two | Three | Four
    deriving (Enum)

instance Show Mood where
    show One = "Uno"
    show Two = "Dos"
    show Three = "Tres"
    show _ = "Que?"