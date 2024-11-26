{-# LANGUAGE InstanceSigs #-}
module Intro where

import Prelude hiding (Semigroup, Monoid)

x :: Integer
x = 42

y :: Integer
y = x * 2

double :: Integer -> Integer
-- double = \ x -> x * 2
double x = x * 2

-- >>> double 7
-- 14

quadruple :: Integer -> Integer
quadruple x =
    let dbl = double x
    in double dbl


octuple :: Integer -> Integer
octuple x =
    let dbl = double x
        quad = double dbl
    in double quad

-- Haustier ist eins der folgenden:
-- - Hund -ODER-
-- - Katze -ODER-
-- - Schlange
data Pet
  = Dog
  | Cat 
  | Snake 
 deriving Show

-- Ist Haustier niedlich?
isCute :: Pet -> Bool

-- >>> isCute Dog
-- True
-- >>> isCute Snake
-- False

-- eine Gleichung pro Fall
isCute Dog = True
isCute Cat = True
isCute Snake = False

-- Gürteltier hat folgende Eigenschaften:
-- - lebendig -ODER- tot
-- - Gewicht

data Liveness = Alive | Dead 
 deriving Show

type Weight = Integer -- Typ-Synonym

{-

-- Record
data Dillo = MkDillo { dilloLiveness :: Liveness,
                       dilloWeight :: Weight }
  deriving Show

-- lebendiges Gürteltier, 10kg
dillo1 :: Dillo
dillo1 = MkDillo { dilloLiveness = Alive, dilloWeight = 10 }

-- totes Gürteltier, 8kg
dillo2 :: Dillo
dillo2 = MkDillo Dead 8

-- >>> dilloLiveness dillo1
-- Alive
-- >>> dilloWeight dillo2
-- 8

-- Gürteltier überfahren
roadkillDillo :: Dillo -> Dillo

-- >>> roadkillDillo dillo1
-- MkDillo {dilloLiveness = Dead, dilloWeight = 10}

-- roadkillDillo dillo = MkDillo { dilloLiveness = Dead, dilloWeight = dilloWeight dillo }
-- roadkillDillo dillo = MkDillo Dead (dilloWeight dillo)
-- roadkillDillo d = d { dilloLiveness = Dead } -- "functional update", Kopie bis auf ...
-- roadkillDillo (MkDillo { dilloLiveness = l, dilloWeight = w}) = MkDillo Dead w
-- roadkillDillo (MkDillo {dilloWeight = w}) = MkDillo Dead w
roadkillDillo (MkDillo _ weight) = MkDillo Dead weight

-}

-- Ein Tier ... ist eins der folgenden:
-- - Gürteltier -ODER-
-- - Papagei

-- algebraischer Datentyp
data Animal =
    MkDillo { dilloLiveness :: Liveness, dilloWeight :: Weight }
  | MkParrot String Weight
  deriving Show

dillo1 :: Animal
dillo1 = MkDillo { dilloLiveness = Alive, dilloWeight = 10 }

dillo2 :: Animal
dillo2 = MkDillo Dead 8

parrot1 :: Animal
parrot1 = MkParrot "hello" 1

-- >>> roadKillAnimal dillo1
-- MkDillo {dilloLiveness = Dead, dilloWeight = 10}

-- >>> roadKillAnimal parrot1
-- MkParrot "" 1

roadKillAnimal :: Animal -> Animal
roadKillAnimal (MkDillo _liveness weight) = MkDillo Dead weight
roadKillAnimal (MkParrot _sentence weight) = MkParrot "" weight

-- Tier füttern

-- >>> feedAnimal dillo1 2
-- MkDillo {dilloLiveness = Alive, dilloWeight = 12}

-- >>> feedAnimal dillo2 2
-- MkDillo {dilloLiveness = Dead, dilloWeight = 8}


-- gibt nur 1stellige Funktionen
feedAnimal :: Animal -> (Weight -> Animal)
feedAnimal (MkDillo liveness weight) amount = 
    case liveness of
        Alive -> MkDillo liveness (weight + amount)
        Dead -> MkDillo liveness weight
feedAnimal (MkParrot sentence weight) amount = MkParrot sentence (weight + amount)

-- Eine geometrische Figur ("Shape") ist eins der folgenden:
-- - Kreis -ODER-
-- - Quadrat -ODER-
-- - Überlagerung zweier geometrischer Figuren

-- 1. Datentypdefinition
-- 2. Funktion, die feststellt, ob ein gegebener Punkt innerhalb 
--    oder außerhalb einer geometrischen Figur liegt
-- Eine (geometrische) Figur ist eins der folgenden:
-- - Kreis -ODER-
-- - Quadrat -ODER-
-- - Überlagerung zweier geometrischer Figuren

-- 1. Datentype(en)
-- 2. Funktion, die bei einem Punkt feststellt,
--    ob er innerhalb oder außerhalb einer geomtrischen Figur liegt

-- data Point = MkPoint Double Double
type Point = (Double, Double)

point1 :: Point
point1 = (1, 1)

point2 :: Point
point2 = (3, 3)

point3 :: Point
point3 = (10, 4)

data Shape
  = MkCircle {center :: Point, radius :: Double}
  | MkSquare {leftBottom :: Point, sideLength :: Double}
  | MkOverlap {shape1 :: Shape, shape2 :: Shape}

circle1 = MkCircle (2, 2) 2.0

square1 = MkSquare (3, 3) 4.0

within :: Shape -> Point -> Bool
within (MkCircle (centerX, centerY) radius) (x, y) =
  let distanceX = (x - centerX) ^ 2
      distanceY = (y - centerY) ^ 2
      difference = sqrt (distanceX + distanceY)
   in difference <= radius
within (MkSquare (squareX, squareY) sideLength) (x, y) =
  let rightTopX = squareX + sideLength
      rightTopY = squareY + sideLength
   in ((x >= squareX) && (x <= rightTopX))
        && ((y >= squareY) && (y <= rightTopY))
within (MkOverlap shape1 shape2) point =
  within shape1 point || within shape2 point
