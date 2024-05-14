{-# LANGUAGE InstanceSigs #-}
module Intro where

import Prelude hiding (Semigroup, Monoid)

x :: Integer
x = 9

-- >>> x * 2
-- 14

inc :: Integer -> Integer
-- inc = \ x -> x + 1
inc x = x + 1 -- syntaktischer Zucker 

-- >>> inc 10
-- 11

foo x y =
    let z = x + y
    in z * 2

-- Ein Haustier ist eins der folgenden:
-- - Hund -ODER-
-- - Katze -ODER-
-- - Schlange -ODER-
-- - Fruchtfliege

data Pet =
    Dog
  | Cat
  | Snake 
  | Fruitfly
  deriving Show

isCute :: Pet -> Bool
-- Schablone: eine Gleichung pro Fall
-- isCute Dog = undefined
-- isCute Cat = undefined
-- isCute Snake = undefined
-- isCute Fruitfly = undefined

isCute Dog = True
isCute Cat = True
isCute Snake = True
isCute Fruitfly = False

-- data Bool = True | False

-- >>> isCute Dog
-- True
-- >>> isCute Fruitfly
-- False

-- Ein Gürteltier hat folgende Eigenschaften:
-- - lebendig -ODER- tot
-- - Gewicht

data Liveness = Alive | Dead
  deriving Show

-- Typ-Alias
type Weight = Integer

{-
data Dillo = 
    MkDillo { -- Konstruktor 
        dilloLiveness :: Liveness, -- Selektor
        dilloWeight :: Weight
    }
    deriving Show

-- lebendiges Gürteltier, 10kg
dillo1 :: Dillo
dillo1 = MkDillo { dilloLiveness = Alive, dilloWeight = 10 }

-- totes Gürteltier, 8kg
dillo2 :: Dillo
dillo2 = MkDillo Alive 10

-- >>> dilloLiveness dillo1
-- Alive

-- >>> dilloWeight dillo2
-- 10

runOverDillo :: Dillo -> Dillo
--- runOverDillo dillo = MkDillo { dilloLiveness = Dead, dilloWeight = dilloWeight dillo }
-- runOverDillo dillo = MkDillo Dead (dilloWeight dillo)
-- runOverDillo (MkDillo { dilloLiveness = liveness, dilloWeight = w}) = -- Pattern matching
--    MkDillo { dilloLiveness = Dead, dilloWeight = w }
-- runOverDillo (MkDillo {dilloWeight = w}) = MkDillo Dead w
-- runOverDillo dillo = dillo { dilloLiveness = Dead } -- functional update, Kopie bis auf einige Felder
runOverDillo (MkDillo _ weight) = MkDillo Dead weight -- _: "don't care"
-}

{-
data Dillo = MkDillo
  { -- Konstruktor
    dilloLiveness :: Liveness, -- Selektor
    dilloWeight :: Weight
  }
  deriving Show

data Animal =
    Dillo -- 

dillo1 = MkDillo {dilloLiveness = Alive, dilloWeight = 10}
-}

-- algebraischer Datentyp
data Animal =
    MkDillo {
        dilloLiveness :: Liveness,
        dilloWeight :: Weight
    }
  | MkParrot String Weight 
  deriving Show

dillo1 :: Animal
dillo1 = MkDillo {dilloLiveness = Alive, dilloWeight = 10}

dillo2 :: Animal
dillo2 = MkDillo Dead 8 

parrot1 :: Animal
parrot1 = MkParrot "hello" 1
parrot2 :: Animal
parrot2 = MkParrot "goodbye" 2

runOverAnimal :: Animal -> Animal
runOverAnimal (MkDillo _ weight) = MkDillo Dead weight
runOverAnimal (MkParrot _ weight) = MkParrot "" weight

-- >>> runOverAnimal dillo1
-- MkDillo {dilloLiveness = Dead, dilloWeight = 10}
-- >>> runOverAnimal parrot1
-- MkParrot "" 1
