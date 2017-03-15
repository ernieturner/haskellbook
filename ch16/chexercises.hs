{-# LANGUAGE FlexibleInstances #-}

module Chexercises where

--1. No, not of kind * -> *
--2.
data BoolAndSomethingElse a = False' a | True' a
-- Yes, has kind * -> *

--3
data BoolAndMaybeSomethingElse a = Falsish | Truish a
-- Yes has kind * -> *

--4. Yes?
--5. No?

--Rearrange
--1
data Sum a b = First a | Second b

instance Functor (Sum e) where
    fmap f (First a) = First a
    fmap f (Second b) = Second (f b)

--2
data Company a b c = DeepBlue a c | Something b
instance Functor (Company e e') where
    fmap _ (Something b) = Something b
    fmap f (DeepBlue a c) = DeepBlue a (f c)

--3
data More b a = L a b a | R b a b deriving (Eq, Show)
instance Functor (More x) where
    fmap f (L a b a') = L (f a) b (f a')
    fmap f (R b a b') = R  b (f a) b'


--Functor Instances
--1
data Quant a b = Finance | Desk a | Bloor b
instance Functor (Quant a) where
    fmap _ Finance = Finance
    fmap _ (Desk a) = Desk a
    fmap f (Bloor b) = Bloor (f b)

--2
data K a b = K a
instance Functor (K a) where
    fmap _ (K a) = K a

--3. Not sure what this is...?
newtype Flip f a b = Flip (f b a) deriving (Eq, Show)
instance Functor (Flip K a) where
    fmap f (Flip (K a)) = Flip (K (f a))

--4
data EvilGoateeConst a b = GoatyConst b
instance Functor (EvilGoateeConst a) where
    fmap f (GoatyConst b) = GoatyConst (f b)

--5
data LiftItOut f a = LiftItOut (f a)
instance Functor f => Functor (LiftItOut f) where
    fmap f (LiftItOut fa) = LiftItOut (fmap f fa)

--6
data Parappa f g a = DaWrappa (f a) (g a)
instance (Functor f, Functor g) => Functor (Parappa f g) where
    fmap f (DaWrappa fa ga) = DaWrappa (fmap f fa) (fmap f ga)

--7
data IgnoreOne f g a b = IgnoringSomething (f a) (g b)
instance Functor g => Functor (IgnoreOne f g a) where
    fmap f (IgnoringSomething fa gb) = IgnoringSomething fa (fmap f gb)

--8
data Notorious g o a t = Notorious (g o) (g a) (g t)
instance Functor g => Functor (Notorious g o a) where
    fmap f (Notorious go ga gt) = Notorious go ga (fmap f gt)

--9
data List a = Nil | Cons a (List a)
instance Functor List where
    fmap _ Nil = Nil
    fmap f (Cons a la) = Cons (f a) (fmap f la)

--10
data GoatLord a = NoGoat | OneGoat a | MoreGoats (GoatLord a) (GoatLord a) (GoatLord a)
instance Functor GoatLord where
    fmap _ NoGoat = NoGoat
    fmap f (OneGoat a) = OneGoat (f a)
    fmap f (MoreGoats a a' a'') = MoreGoats (fmap f a) (fmap f a') (fmap f a'')

--11
data TalkToMe a = Halt | Print String a | Read (String -> a)
instance Functor TalkToMe where
    fmap _ Halt = Halt
    fmap f (Print a b) = Print a (f b)
    fmap f (Read sa) = Read (fmap f sa)


