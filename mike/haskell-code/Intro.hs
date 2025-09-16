{-# LANGUAGE InstanceSigs #-}
module Intro where

x :: Integer
x = 42

y :: Integer
y = x + 2

double :: Integer -> Integer
-- double = \x -> x * 2
double x = x * 2

-- >>> double 21
-- 42

quadruple :: Integer -> Integer
quadruple x =
    let d = double x
    in double d

-- >>> quadruple 21 
-- 84
 
 {- Blockkommentar
 -}

-- Haustier ist eins der folgenden:
-- - Hund ODER
-- - Katze ODER
-- - Schlange
data Pet =
    Dog
  | Cat
  | Snake
  deriving Show -- hinter jedes data

-- Abseitsregel: Folgezeilen eines mehrzeilgen Konstrukts müssen eingerückt werden

-- Ist ein Haustier niedlich?
isCute :: Pet -> Bool
-- Schablone: eine Gleichung pro Fall
-- isCute Dog = undefined
-- isCute Cat = undefined
-- isCute Snake = undefined

-- >>> isCute Dog
-- True
-- >>> isCute Snake
-- False

isCute Dog = True
isCute Cat = True
isCute Snake = False

-- geht auch, aber !/&""%§&!"§"
-- isCute Snake = False
-- isCute otherPet = True
-- isCute _ = True

-- Gürteltier hat folgende Eigenschaften:
-- - lebendig ODER tot  UHND
-- - Gewicht

data Liveness = Dead | Alive
  deriving Show

-- Typsynonym
type Weight = Integer

data Dillo = MkDillo { dilloLiveness :: Liveness,
                       dilloWeight :: Weight }
  deriving Show

dillo1 :: Dillo
dillo1 = MkDillo { dilloLiveness = Alive, dilloWeight = 10 }

dillo2 :: Dillo
dillo2 = MkDillo Dead 8

-- runOverDillo :: Dillo -> Dillo
