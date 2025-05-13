{-# LANGUAGE InstanceSigs #-}
module Intro where

import Prelude hiding (Semigroup, Monoid)

x :: Integer
x = 23 + 42 * 2

y :: Integer
y = x * 2

-- Zahl verdoppeln
double :: Integer -> Integer
-- >>> double 5 
-- 10

-- double = \ x -> x * 2
double x = x * 2 -- syntaktischer Zucker

-- Abseitsregel: Wenn ein Konstrukt über mehrere Zeilen geht,
-- müssen die Folgezeilen gegenüber der ersten Zeile eingerückt
-- sein.

-- Haustier ist eins der folgenden:
-- - Hund -ODER-
-- - Katze -ODER-
-- - Schlange
data Pet = -- data: neuer Datentyp
    Dog -- "Konstruktor"
  | Cat
  | Snake
  deriving Show -- Zauberspruch

-- Ist Haustier niedlich?
isCute :: Pet -> Bool
-- 1 Gleichung pro Fall
-- Schablone:
-- isCute Dog = undefined
-- isCute Cat = undefined
-- isCute Snake = undefined

-- >>> isCute Dog
-- True
-- >>> isCute Snake
-- False

-- Pattern-Matching:
isCute Dog = True
isCute Cat = True
isCute Snake = False
-- isCute pet =
--    case pet of
--        Dog -> True
--        Cat -> True
--        Snake -> False

-- Gürteltier hat folgende Eigenschaften:
-- - lebendig ODER tot
-- - Gewicht

data Liveness = Alive | Dead deriving Show

type Weight = Integer -- Typsynonym

-- Tier auf dem texanischen Highway:
-- - Gürteltier -ODER-
-- - Schlange

{-
data Dillo = 
    MkDillo { dilloLiveness :: Liveness, -- MkDillo: Konstruktor, dilloLiveness ... Selektoren
              dilloWeight :: Weight }
    deriving Show

-- lebendiges Gürteltier 10kg
dillo1 :: Dillo
dillo1 = MkDillo { dilloLiveness = Alive, dilloWeight = 10}

-- totes Gürteltier, 8kg
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

-- runOverDillo dillo =
--    MkDillo {dilloLiveness = Dead, dilloWeight = dilloWeight dillo }
-- runOverDillo dillo = MkDillo Dead (dilloWeight dillo)
-- runOverDillo (MkDillo { dilloLiveness = l, dilloWeight = w }) = -- Pattern
--    MkDillo { dilloLiveness = Dead, dilloWeight = w }
-- runOverDillo (MkDillo _liveness weight) = MkDillo Dead weight -- _: "don't care"
runOverDillo dillo = dillo { dilloLiveness = Dead } -- "functional update", Kopie bis auf ...

-- nur 1stellige Funktionen in Haskell
feedDillo :: Dillo -> Weight -> Dillo
feedDillo dillo amount =
    case dilloLiveness dillo of
        Alive -> dillo { dilloWeight = dilloWeight dillo + amount }
        Dead -> dillo

feedDillo' :: Weight -> Dillo -> Dillo
feedDillo' amount dillo =
    case dilloLiveness dillo of
      Alive -> dillo {dilloWeight = dilloWeight dillo + amount}
      Dead -> dillo

-- Eingaben vertauschen
-- swap :: (Dillo -> Weight -> Dillo) -> (Weight -> Dillo -> Dillo)

-- >>> (swap feedDillo) 5 dillo1
-- MkDillo {dilloLiveness = Alive, dilloWeight = 15}

swap :: (a -> b -> c) -> (b -> a -> c)
-- swap f = \ weight -> \ dillo -> f dillo weight
-- swap f = \b -> \a -> f a b
swap f b a = f a b
-- eingebaut flip

-- Tupel: Ad-hoc zusammengesetzte Daten

-- >>> feedDillo''(dillo1, 5)
-- MkDillo {dilloLiveness = Alive, dilloWeight = 15}

feedDillo'' :: (Dillo, Weight) -> Dillo
feedDillo''(dillo, amount) =
  case dilloLiveness dillo of
    Alive -> dillo {dilloWeight = dilloWeight dillo + amount}
    Dead -> dillo

-- Haskell Curry -> curry, curryfizieren
-- Moses Schönfinkel -> schönfinkeln

-- eingebaut: curry, uncurry

-- a, b, c: Typvariablen (kleingeschrieben)
entschönfinkeln :: (a -> b -> c) -> ((a, b) -> c)

-- >>> (tuplify feedDillo) (dillo1, 5)
-- MkDillo {dilloLiveness = Alive, dilloWeight = 15}

-- tuplify f = \ (a, b) -> f a b
entschönfinkeln f (a, b) = f a b

schönfinkeln :: ((a, b) -> c) -> a -> b -> c
schönfinkeln f a b = f (a, b)
-}

type Length = Integer
type Thickness = Integer


data Animal =
    MkDillo { dilloLiveness :: Liveness, dilloWeight :: Weight}
  | MkSnake Length Thickness -- nur verwendbar mit Pattern-Matching, abgekürzten Notation
  deriving Show

dillo1 :: Animal
dillo1 = MkDillo { dilloLiveness = Alive, dilloWeight = 10 }

dillo2 :: Animal
dillo2 = MkDillo Dead 8

snake1 :: Animal
snake1 = MkSnake 300 10

runOverAnimal :: Animal -> Animal

-- >>> runOverAnimal dillo1
-- MkDillo {dilloLiveness = Dead, dilloWeight = 10}

-- >>> runOverAnimal snake1
-- MkSnake 300 0

-- jede Gleichung muß Konstruktor erwähnen
runOverAnimal (MkDillo _ weight) = MkDillo Dead weight
runOverAnimal (MkSnake length _) = MkSnake length 0

-- Geometrische Figur ("shape") ist eins der folgenden:
-- -- Kreis -ODER-
-- -- Quadrat -ODER-
-- -- Überlagerung zweiter geometrischer Figuren

-- Aufgabe:
-- 1. Datenanalyse, Repräsentation
-- 2. Funktion, die für einen Punkt feststellt, ob er innerhalb einer geometrischen Figur liegt

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
