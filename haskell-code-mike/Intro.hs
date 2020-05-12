{-# OPTIONS_GHC -fwarn-incomplete-patterns #-}
module Intro where

x :: Integer
x = 5

-- Aggregatzustand
data State = -- data: neuer Datentyp
  Solid | Liquid | Gas
  deriving (Show, Eq) -- generiert Typklassen-Instanzen für Show, Eq

s1 :: State
s1 = Solid
s2 :: State
s2 = Liquid
s3 :: State
s3 = Gas

-- Aggregatzustand von Wasser ausrechnen
waterState :: Integer -> State
waterState temp =
  if temp < 0
  then Solid
  else if temp <= 100
  then Liquid
  else Gas

waterState' temp
  | temp < 0    = Solid
  | temp <= 100 = Liquid
  | otherwise   = Gas

-- algebraischer Datentyp: 3 Klassen / 3 Konstruktoren
-- Typen, Konstruktoren: Großbuchstaben
data Pet = Hund | Katze | Schlange
  deriving (Show, Eq)

-- data Bool = True | False

-- Ist Haustier niedlich?
isCute :: Pet -> Bool

isCute Hund = True
isCute Katze = True
-- isCute Schlange = False

-- Tiere auf dem texanischen Highway

-- Gürteltiere
-- Konstruktor: Pflicht

data Dillo = Dillo1 { alive :: Bool, weight :: Integer }
        --   ^^^^^ Konstruktor, heißt genauso wie der Typ
 deriving Show

-- Satz, Gewicht
{-
data Parrot = Parrot String Integer
  deriving Show
-}
-- funktioniert nicht: data Animal = Dillo | Parrot

data Animal =
    Dillo { dilloAlive :: Bool, dilloWeight :: Integer }
  | Parrot String Integer
  deriving (Show, Eq)

dillo1 = Dillo { dilloAlive = True, dilloWeight = 10 }
dillo2 = Dillo False 12

-- Tier überfahren
runOverAnimal :: Animal -> Animal
-- runOverAnimal dillo@(Dillo False weight) = dillo
-- runOverAnimal (Dillo _ weight) = Dillo False weight
-- runOverAnimal (Dillo { dilloAlive = alive, dilloweight = weight}) = undefined
-- runOverAnimal dillo@(Dillo {}) = Dillo False (dilloWeight dillo)
runOverAnimal dillo@(Dillo {}) = dillo { dilloAlive = False }
runOverAnimal (Parrot sentence weight) = Parrot "" weight