{-# LANGUAGE GADTs #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE NoImplicitPrelude #-}
module Data.TypeVector where

import Prelude hiding (head, tail, length, init, last)

data Nat = Zero | Succ Nat

type One = 'Succ 'Zero
type Two = 'Succ One
type Three = 'Succ Two

type family Plus (n :: Nat) (m :: Nat) where
  Plus 'Zero n = n
  Plus ('Succ n) m = 'Succ (Plus n m)

data Vector :: Nat -> * -> * where
  Nil :: Vector 'Zero a
  (:>) :: a -> Vector n a -> Vector ('Succ n) a

infixr 5 :>

instance Show a => Show (Vector n a) where
  show vec = show' vec True
    where
      show' :: Show a => Vector n a -> Bool -> String
      show' (a :> b) True = "<" ++ show a ++ show' b False
      show' Nil True = "<>"
      show' (a :> b) False = "," ++ show a ++ show' b False
      show' Nil False = ">"

instance Functor (Vector n) where
  fmap f (x :> xs) = f x :> fmap f xs
  fmap _ Nil = Nil
instance Eq a => Eq (Vector n a) where
  (==) (x :> xs) (y :> ys) = x == y && xs == ys
  (==) Nil Nil = True
  (==) _ _ = error "Impossible because both vectors are length n"
  
applyVector :: Vector n (a -> b) -> Vector n a -> Vector n b
applyVector (f :> fs) (x :> xs) = f x :> applyVector fs xs
applyVector Nil Nil = Nil
applyVector _ _ = error "Impossible because both vectors are length n"

addVector :: Num a => Vector n a -> Vector n a -> Vector n a
addVector (a :> veca) (b :> vecb) = a + b :> addVector veca vecb
addVector Nil Nil = Nil
addVector _ _ = error "Impossible because both vectors are length n"

fromValue :: a -> Vector One a
fromValue x = x :> Nil

fromPair :: (a,a) -> Vector Two a
fromPair (a,b) = a :> b :> Nil

toList :: Vector n a -> [a]
toList Nil = []
toList (x :> xs) = x : toList xs

uncons :: Vector ('Succ n) a -> (a, Vector n a)
uncons (x :> xs) = (x,xs)

head :: Vector ('Succ n) a -> a
head (x :> _) = x

tail :: Vector ('Succ n) a -> Vector n a
tail (_ :> xs) = xs

last :: Vector ('Succ n) a -> a
last (x :> Nil) = x
last (_ :> xs@(_ :> _)) = last xs

init :: Vector ('Succ n) a -> Vector n a
init (_ :> Nil) = Nil
init (x :> xs@(_ :> _)) = x :> init xs

append :: Vector n a -> Vector m a -> Vector (Plus n m) a
append (x :> xs) ys = x :> append xs ys
append Nil ys = ys

dotProduct :: Num a => Vector n a -> Vector n a -> a
dotProduct (x :> xs) (y :> ys) = x * y + dotProduct xs ys
dotProduct Nil Nil = 0
dotProduct _ _ = error "Impossible because both vectors are length n"

crossProduct :: Num a => Vector Three a -> Vector Three a -> Vector Three a
crossProduct (a1:>a2:>a3:>Nil) (b1:>b2:>b3:>Nil) =
  (a2 * b3 - a3 * b2) :> (a3 * b1 - a1 * b3) :> (a1 * b2 - a2 * b1) :> Nil
crossProduct _ _ = error "Impossible because both vectors are length 3"

length :: Num q => Vector n a -> q
length (_ :> xs) = 1 + length xs
length Nil = 0

null :: Vector n a -> Bool
null Nil = True
null (_ :> _) = False
