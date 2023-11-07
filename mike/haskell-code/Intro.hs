{-# LANGUAGE InstanceSigs #-}
{-# OPTIONS_GHC -fwarn-incomplete-patterns #-}
module Intro where

import Prelude hiding (Semigroup, Monoid)

x :: Integer
x = 8

-- Zeilenkommentar

-- >>> x * 2
-- 16

inc :: Integer -> Integer
-- >>> inc 5
-- 6

-- inc x = x + 1
inc = \ x -> x + 1

-- Ein Haustier ist eins der folgenden:
-- - Hund
-- - Katze
-- - Schlange
data Pet = Dog | Cat | Snake
  deriving Show

-- Ist ein Haustier niedlich?
isCute :: Pet -> Bool
-- >>> isCute Dog
-- True
-- >>> isCute Snake
-- False

-- eine Gleichung pro Fall
isCute Dog = True
isCute Cat = True
isCute Snake = False

-- Ein Gürteltier hat folgende Eigenschaften:
-- - lebendig -ODER- tot
-- - Gewicht

data Liveness = Alive | Dead
  deriving Show 

-- Typsynonym
type Weight = Integer

{-
data Dillo =
    MkDillo { dilloLiveness :: Liveness, dilloWeight :: Weight }
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
-- >>> runOverDillo dillo1
-- MkDillo {dilloLiveness = Dead, dilloWeight = 10}

-- runOverDillo dillo = MkDillo { dilloLiveness = Dead, dilloWeight = dilloWeight dillo }
-- runOverDillo dillo = MkDillo Dead (dilloWeight dillo)
-- runOverDillo (MkDillo { dilloLiveness = l, dilloWeight = w}) =
--    MkDillo { dilloLiveness = Dead, dilloWeight = w }
-- runOverDillo (MkDillo _ w) = MkDillo Dead w
-- runOverDillo (MkDillo { dilloWeight = w}) =
--    MkDillo { dilloLiveness = Dead, dilloWeight = w }
-- "functional update"
runOverDillo dillo = dillo { dilloLiveness = Dead }
-}

-- Ein Tier ist eins der folgenden:
-- - Gürteltier -ODER-
-- - Papagei
-- algebraischer Datentyp
data Animal =
    MkDillo { dilloLiveness :: Liveness, dilloWeight :: Weight }
  | MkParrot String Weight
  deriving Show

dillo1 :: Animal
dillo1 = MkDillo Alive 10

dillo2 :: Animal
dillo2 = MkDillo Dead 8

parrot1 :: Animal
parrot1 = MkParrot "Hallo!" 1
parrot2 :: Animal
parrot2 = MkParrot "Tschüss!" 2

-- Tier überfahren
runOverAnimal :: Animal -> Animal
-- eine Gleichung pro Fall

-- >>> runOverAnimal parrot1
-- MkParrot "" 1
-- >>> runOverAnimal dillo1
-- MkDillo {dilloLiveness = Dead, dilloWeight = 10}

-- Alias-Pattern
runOverAnimal dillo@(MkDillo liveness weight) = 
    case liveness of
        Dead -> dillo
        Alive -> MkDillo Dead weight
-- runOverAnimal (MkDillo Alive weight) = MkDillo Dead weight
-- runOverAnimal dillo@(MkDillo Dead weight) = dillo
runOverAnimal (MkParrot sentence weight) = MkParrot "" weight

-- Tier füttern
feedAnimal dillo@(MkDillo liveness weight) amount =
    case liveness of
        Alive -> MkDillo liveness (weight + amount)
        Dead -> dillo
feedAnimal (MkParrot sentence weight) amount =
    MkParrot sentence (weight + amount)