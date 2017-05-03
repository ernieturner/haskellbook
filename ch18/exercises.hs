module Exercises where

import Control.Applicative
import Data.Monoid

--1
data Nope a = NopeDotJpg deriving (Show, Eq)

instance Functor Nope where
    fmap _ NopeDotJpg = NopeDotJpg

instance Applicative Nope where
    pure x = NopeDotJpg
    _ <*> _ = NopeDotJpg

instance Monad Nope where
    return = pure
    NopeDotJpg >>= _ = NopeDotJpg

-- 2
data PhhhbbtttEither b a = Lefty a | Righty b deriving (Show, Eq)

instance Functor (PhhhbbtttEither b) where
    fmap _ (Righty b) = Righty b
    fmap f (Lefty a) = Lefty (f a)

instance Applicative (PhhhbbtttEither b) where
    pure x = Lefty x
    _ <*> (Righty b) = Righty b
    (Righty b) <*> _ = Righty b
    (Lefty f) <*> (Lefty a) = Lefty (f a)

instance Monad (PhhhbbtttEither b) where
    return = pure
    (Righty b) >>= _ = Righty b
    (Lefty a) >>= f = f a

-- 3
newtype Identity a = Identity a deriving (Eq, Ord, Show)

instance Functor Identity where
    fmap f (Identity a) = Identity (f a)

instance Applicative Identity where
    pure = Identity
    (Identity f) <*> (Identity a) = Identity (f a)

instance Monad Identity where
    return = pure
    (Identity a) >>= f = f a

-- 4
data List a = Nil | Cons a (List a) deriving (Show, Eq)

instance Functor List where
    fmap _ Nil = Nil
    fmap f (Cons a la) = Cons (f a) (fmap f la)

append' :: List a -> List a -> List a
append' Nil a = a
append' a Nil = a
append' (Cons x xs) y = Cons x (append' xs y)

instance Applicative List where
    pure x = Cons x Nil
    _ <*> Nil = Nil
    Nil <*> _ = Nil
    (Cons f fa) <*> lb = (fmap f lb) `append'` (fa <*> lb)

instance Monad List where
    return = pure
    Nil >>= _ = Nil
    (Cons x xs) >>= f = f x `append'` (xs >>= f)



meh :: Monad m => [a] -> (a -> m b) -> m [b]
meh [] _ = pure []
meh (x:xs) f = do
    b <- f x
    bs <- meh xs f
    return (b:bs)

meh' :: Monad m => [a] -> (a -> m b) -> m [b]
meh' [] _ = pure []
meh' (x:xs) f = (:) <$> (f x) <*> (meh' xs f)

flipType :: (Monad m) => [m a] -> m [a]
flipType xxs = meh xxs id

flipType' :: (Monad m) => [m a] -> m [a]
flipType' = flip meh id
