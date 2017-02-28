module Exercises where

import Control.Monad
import Data.Monoid
import Test.QuickCheck

data Optional a = Nada | Only a deriving (Eq, Show)

instance Monoid a => Monoid (Optional a) where
    mempty = Nada
    mappend (Only a) (Only b) = Only (a <> b)
    mappend (Only a) _ = Only a
    mappend _ (Only a) = Only a
    mappend _ _ = Nada


type Verb = String
type Adjective = String
type Adverb = String
type Noun = String
type Exclamation = String

madlibbin :: Exclamation
          -> Adverb
          -> Noun
          -> Adjective
          -> String
madlibbin e adv noun adj =
    e <> "! he said " <>
    adv <> " as he jumped into his car " <>
    noun <> " and drove off with his " <>
    adj <> " wife."

madlibbinplus :: Exclamation
          -> Adverb
          -> Noun
          -> Adjective
          -> String
madlibbinplus e adv noun adj =
    mconcat [e, "! he said ", adv, " as he jumped into his car ", noun, " and drove off with his ", adj, " wife."]



--monoidAssoc :: (Eq m, Monoid m) => m -> m -> m -> Bool
--monoidAssoc a b c = (a <> (b <> c)) == ((a <> b) <> c)

--monoidLeftIdentity :: (Eq m, Monoid m) => m -> Bool
--monoidLeftIdentity a = (mempty <> a) == a

--monoidRightIdentity :: (Eq m, Monoid m) => m -> Bool
--monoidRightIdentity a = (a <> mempty) == a

newtype First' a = First' { getFirst' :: Optional a } deriving (Eq, Show)

instance Monoid (First' a) where
    mempty = First' Nada
    mappend (First' Nada) (First' a) = First' a
    mappend (First' a) (First' Nada) = First' a
    mappend (First' a) (First' b)    = First' a


--firstMappend :: First' a -> First' a -> First' a
--firstMappend = mappend

--type FirstMappend =
--     First' String
--  -> First' String
--  -> First' String
--  -> Bool

--type FstId = First' String -> Bool