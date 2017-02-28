module Exercises where

import Test.QuickCheck (Arbitrary, arbitrary, quickCheck)
import Data.Semigroup (Semigroup, (<>))
import Data.Monoid (Monoid)


-- TRIVIAL
data Trivial = Trivial deriving (Eq, Show)

instance Semigroup Trivial where
    _ <> _ = Trivial

instance Monoid Trivial where
    mempty = Trivial
    mappend = (<>)

instance Arbitrary Trivial where
    arbitrary = return Trivial


-- IDENTITY
newtype Identity a = Identity a deriving (Eq, Show)

instance Semigroup a => Semigroup (Identity a) where
  (Identity a) <> (Identity b) = Identity (a <> b)

instance Monoid a => Monoid (Identity a) where
    mempty = Identity mempty
    mappend (Identity a) (Identity b) = Identity (mappend a b)

instance Arbitrary a => Arbitrary (Identity a) where
  arbitrary = do
    a <- arbitrary
    return (Identity a)

--TWO
data Two a b = Two a b deriving (Eq, Show)

instance (Semigroup a, Semigroup b) => Semigroup (Two a b) where
    (Two a b) <> (Two c d) = Two (a <> c) (b <> d)

instance (Monoid a, Monoid b) => Monoid (Two a b) where
    mempty = Two mempty mempty
    mappend (Two a b) (Two c d) = (Two (mappend a c) (mappend b d))

instance (Arbitrary a, Arbitrary b) => Arbitrary (Two a b) where
    arbitrary = do
        a <- arbitrary
        b <- arbitrary
        return (Two a b)

--THREE
data Three a b c = Three a b c deriving (Eq, Show)

instance (Semigroup a, Semigroup b, Semigroup c) => Semigroup (Three a b c) where
    (Three a b c) <> (Three d e f) = Three (a <> d) (b <> e) (c <> f)

instance (Arbitrary a, Arbitrary b, Arbitrary c) => Arbitrary (Three a b c) where
    arbitrary = do
        a <- arbitrary
        b <- arbitrary
        c <- arbitrary
        return (Three a b c)


-- BOOLCONJ
newtype BoolConj = BoolConj Bool deriving (Eq, Show)

instance Semigroup BoolConj where
    (BoolConj a) <> (BoolConj b) = BoolConj (a && b)


semigroupAssoc :: (Eq m, Semigroup m) => m -> m -> m -> Bool
semigroupAssoc a b c = (a <> (b <> c)) == ((a <> b) <> c)

monoidLeftIdentity :: (Eq m, Monoid m, Semigroup m) => m -> Bool
monoidLeftIdentity a = (mempty <> a) == a

monoidRightIdentity :: (Eq m, Monoid m, Semigroup m) => m -> Bool
monoidRightIdentity a = (a <> mempty) == a

type TrivialAssoc = Trivial -> Trivial -> Trivial -> Bool
type IdentityAssoc = Identity String -> Identity String -> Identity String -> Bool
type TwoAssoc = Two String Ordering -> Two String Ordering -> Two String Ordering -> Bool
type ThreeAssoc = Three String String String -> Three String String String -> Three String String String -> Bool

main :: IO ()
main = do
    quickCheck (semigroupAssoc :: TrivialAssoc)
    quickCheck (semigroupAssoc :: IdentityAssoc)
    quickCheck (semigroupAssoc :: TwoAssoc)
    quickCheck (semigroupAssoc :: ThreeAssoc)