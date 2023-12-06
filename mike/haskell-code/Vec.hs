{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE GADTs #-}

{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE DerivingStrategies #-}
-- {-# LANGUAGE FlexibleContexts #-}

{-# OPTIONS_GHC -fwarn-incomplete-patterns #-}

module Vec where

import Data.Kind (Type)
import Prelude (Show(..), Num(..), Functor(..), id, ($), Bool(..), Integer, Double, (&&), undefined)

data Nat = Zero | Succ Nat
  deriving Show

zero :: Nat
zero = Zero

one :: Nat
one = Succ Zero

two :: Nat
two = Succ one

three = Succ two

add :: Nat -> Nat -> Nat
add Zero n = n
add (Succ m') n = Succ (add m' n)

type Vec :: Nat -> Type -> Type
data Vec n a where
  Nil  :: Vec Zero a
  (:>) :: a -> Vec n a -> Vec (Succ n) a

infixr 5 :>

deriving stock instance Show a => Show (Vec n a)

vec1, vec2 :: Vec (Succ (Succ (Succ Zero))) Double
vec1 = 1 :> 2 :> 3 :> Nil
vec2 = 3 :> 4 :> 5 :> Nil

type SNat :: Nat -> Type
data SNat n where
  SZero :: SNat Zero
  SSucc :: SNat n -> SNat (Succ n)

length' :: Vec n a -> Nat
length' Nil = Zero
length' (_ :> xs) = Succ (length' xs)

length :: Vec n a -> SNat n
length Nil = SZero
length (_ :> xs) = SSucc (length xs)

replicate :: SNat n -> a -> Vec n a
replicate SZero _ = Nil
replicate (SSucc n) x = x :> replicate n x

head :: Vec (Succ n) a -> a
head (x :> _) = x

all :: (a -> Bool) -> Vec n a -> Bool
all p Nil = True
all p (x :> xs) = (p x) && (all p xs)

sum :: Vec n Double -> Double
sum Nil = 0
sum (x :> xs) = x + (sum xs)

type Matrix m n a = Vec m (Vec n a)

m1 :: Matrix (Succ (Succ Zero)) (Succ (Succ (Succ Zero))) Double
m1 = vec1 :> vec2 :> Nil


map :: (a -> b) -> Vec n a -> Vec n b
map f Nil = Nil
map f (x :> xs) = f x :> map f xs

(<$>) = map

zipWith :: (a -> b -> c) -> Vec n a -> Vec n b -> Vec n c
zipWith f Nil Nil = Nil
zipWith f (x :> xs) (y :> ys) = (f x y) :> zipWith f xs ys

ap :: Vec n (a -> b) -> Vec n a -> Vec n b
ap fs xs = zipWith ($) fs xs

-- repeat :: Vec n a


-- transpose :: Matrix (Succ m') (Succ n') a -> Matrix (Succ n') (Succ m') a
transpose :: (m ~ Succ m') => Matrix m n a -> Matrix n m a
-- transpose :: Matrix m n a -> Matrix n m a
-- transpose ((x :> xs) :> xss) = (x :> (map (\ (h :> _) -> h) xss)) :> (transpose (xs :> (map (\ (_ :> t) -> t) xss)))
-- transpose = traverse id

transpose (x :> Nil) = map (\ b -> b :> Nil) x
transpose (x :> xs@(y :> ys)) = zipWith (:>) x (transpose xs)


traverse :: (m ~ Succ m', n ~ Succ n') => (a -> Vec m b) -> Vec n a -> Vec m (Vec n b)
-- traverse :: (a -> Vec (Succ m') b) -> Vec (Succ n') a -> Vec (Succ m') (Vec (Succ n') b)
-- traverse _ Nil           = pure Nil
traverse f (x :> Nil) = map (\ b -> b :> Nil) (f x)
traverse f (x :> xs@(y :> ys)) = map (:>) (f x) `ap` traverse f xs


-- traverse :: forall a f b n . Applicative f => (a -> f b) -> Vec n a -> f (Vec n b)
-- traverse _ Nil           = pure Nil
-- traverse f (x :> xs) = fmap (:>) (f x) <*> traverse f xs

-- transpose :: Matrix (Succ m') (Succ n') a -> Matrix (Succ n') (Succ m') a
-- transpose = traverse id

-- mmult :: Num a => [[a]] -> [[a]] -> [[a]] 
-- mmult a b = [[ sum $ zipWith (*) ar bc | bc <- (transpose b)] | ar <- a ]

-- transpose :: Matrix m n a -> Matrix n m a
-- transpose 

multiply :: (n ~ Succ n')  => Matrix m n Double -> Matrix n p Double -> Matrix m p Double
multiply a b =
  map (\ ar -> (map (\ bc -> sum (zipWith (*) ar bc)) (transpose b))) a

  
