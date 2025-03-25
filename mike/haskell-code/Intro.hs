{-# LANGUAGE InstanceSigs #-}
module Intro where

x :: Integer
x = 12 + 23 * 2

-- Intuition: "Konstanten" groß, "Variablen" klein
double :: Integer -> Integer
-- double = \ x -> x * 2
double x = x * 2

-- Kommentar
-- >>> double 18 
-- 36

quadruple :: Integer -> Integer
quadruple x =
    let d = double x
    in d * d

-- >>> quadruple 4 
-- 64

data Pet
  = Dog
  | Cat
  | Snake
  deriving Show

-- Ist Haustier niedlich?
isCute :: Pet -> Bool
-- eine Gleichung pro Fall
-- Schablone:
-- isCute Dog = undefined
-- isCute Cat = undefined
-- isCute Snake = undefined

-- isCute pet =
--     case pet of
--         Dog -> True
--         Cat -> True
--         Snake -> False

isCute Dog = True
isCute Cat = True
isCute Snake = False

-- >>> isCute Dog
-- True
-- >>> isCute Snake
-- False

-- Gürteltier hat folgende Eigenschaften:
-- -- lebendig oder tot
-- -- Gewicht

data Liveness = Alive | Dead
  deriving Show

-- Typalias
type Weight = Integer

data Dillo = MkDillo { dilloLiveness :: Liveness,
                       dilloWeight :: Weight }
  deriving Show

dillo1 :: Dillo
dillo1 = MkDillo { dilloLiveness = Alive, dilloWeight = 10 }

dillo2 :: Dillo
dillo2 = MkDillo Dead 8

-- >>> dilloLiveness dillo1
-- Alive

-- >>> dilloWeight dillo2
-- 8

-- Gürteltier überfahren
runOverDillo :: Dillo -> Dillo
runOverDillo dillo =
-- Schablone:
--     MkDillo { dilloLiveness = undefined, dilloWeight = undefined }
   MkDillo {dilloLiveness = Dead, dilloWeight = dilloWeight dillo}