{-# LANGUAGE InstanceSigs #-}
module Intro where

x :: Integer
x = 5

y :: Integer
y = x * 2

-- Zeilenkommentar
-- >>> x + y
-- 15

double :: Integer -> Integer
-- double = \ x -> x * 2
double x = x * 2

-- >>> double 7
-- 14

foo x y =
 let z = x + y
     a = z * 2 
    in z * 4 + a

-- Ein Haustier ist eins der folgenden:
-- - Hund -ODER-
-- - Katze -ODER-
-- - Schlange

-- algebraischer Datentyp / Aufzählung
data Pet =
    Dog 
  | Cat
  | Snake
  deriving Show

-- Ist Haustier niedlich?
isCute :: Pet -> Bool
-- eine Gleichung pro Fall
isCute Dog = True
isCute Cat = True
isCute Snake = False

-- >>> isCute Dog
-- True

-- >>> isCute Snake
-- False

-- Gürteltier hat folgende Eigenschaften:
-- - lebendig -ODER- tot -UND-
-- - Gewicht
-- zusammengesetzte Daten

-- Typsynonym
type Weight = Integer

data Liveness = Alive | Dead 
  deriving Show

{-
-- Record
data Dillo =
    MkDillo { dilloLiveness :: Liveness,
              dilloWeight :: Weight }
    deriving Show

-- lebendiges Gürteltier, 10kg
dillo1 :: Dillo
dillo1 = MkDillo { dilloLiveness = Alive, dilloWeight = 10 }

dillo2 :: Dillo
-- dillo2 = MkDillo { dilloLiveness = Dead, dilloWeight = 8 }
dillo2 = MkDillo Dead 8

-- >>> dilloLiveness dillo1
-- Alive
-- >>> dilloWeight dillo2
-- 8

-- Gürteltier überfahren
runOverDillo :: Dillo -> Dillo
-- runOverDillo dillo = MkDillo { dilloLiveness = Dead, 
--                               dilloWeight = dilloWeight dillo }
-- runOverDillo dillo = MkDillo Dead (dilloWeight dillo)
-- runOverDillo (MkDillo { dilloLiveness = l, dilloWeight = w }) =
--    MkDillo Dead w
--- runOverDillo (MkDillo { dilloWeight = w }) =
--    MkDillo Dead w
-- runOverDillo (MkDillo _ weight) = MkDillo Dead weight -- - "don't care"
runOverDillo dillo = dillo { dilloLiveness = Dead } -- Kopie von dillo, bis auf ... "functional update"


-- >>> runOverDillo dillo1
-- MkDillo {dilloLiveness = Dead, dilloWeight = 10}
-}

-- Ein Tier (auf dem texanischen Highway) ist eins der folgenden:
-- - Gürteltier -ODER-
-- - Papagei
-- Fallunterscheidung

-- Ein Papagei hat folgende Eigenschaften:
-- - Satz
-- - Gewicht
data Animal =
     MkDillo { dilloLiveness :: Liveness,
               dilloWeight :: Weight }
   | MkParrot String Weight  
   deriving Show

dillo1 :: Animal
dillo1 = MkDillo { dilloLiveness = Alive, dilloWeight = 10 }
dillo2 :: Animal
dillo2 = MkDillo Dead 8
parrot1 :: Animal
parrot1 = MkParrot "Hello!" 1
parrot2 :: Animal
parrot2 = MkParrot "Goodbye!" 2

-- Tier überfahren
runOverAnimal :: Animal -> Animal
runOverAnimal (MkDillo _liveness weight) =
  MkDillo Dead weight
runOverAnimal (MkParrot _sentence weight) =
  MkParrot "" weight

-- Datenmodell für die Karten des französischen Blatts

-- Yaron Minsky: "Make illegal states unrepresentable."

bar :: Double -> Double -> Double
-- bar x y = (x + y)/2
bar = \ x -> \ y -> (x + y)/2

feedAnimal :: Animal -> (Weight -> Animal)
feedAnimal dillo@(MkDillo liveness weight) amount = -- alias pattern
  case liveness of
    Alive -> MkDillo liveness (weight + amount)
    Dead -> dillo -- MkDillo liveness weight
feedAnimal (MkParrot sentence weight) amount = 
  MkParrot sentence (weight + amount)

-- eingebaut als "flip"
-- swap :: (Animal -> Weight -> Animal) -> (Weight -> Animal -> Animal)
swap :: (a -> b -> c) -> (b -> a -> c) -- Typvariablen
-- swap f = \ b -> \ a -> f a b
swap f b a = f a b

-- feedAnimal' :: Weight -> Animal -> Animal
-- feedAnimal' = swap feedAnimal

feedAnimal' :: (Animal, Weight) -> Animal
feedAnimal'(dillo@(MkDillo liveness weight), amount) =
  -- alias pattern
  case liveness of
    Alive -> MkDillo liveness (weight + amount)
    Dead -> dillo -- MkDillo liveness weight
feedAnimal'(MkParrot sentence weight, amount) =
  MkParrot sentence (weight + amount)

-- Haskell Curry
-- Moses Schönfinkel

-- "entschönfinkeln"
-- eingebaut als "uncurry"
tuplify :: (a -> b -> c) -> ((a, b) -> c)
-- tuplify f = \ (a, b) -> f a b
tuplify f (a, b) = f a b

-- "schönfinkeln"
-- eingebaut als "curry"
untuplify :: ((a, b) -> c) -> (a -> b -> c)
-- untuplify f = \a -> \b -> f (a, b)
untuplify f a b = f (a, b)

-- Funktionskomposition
-- eingebaut als . (Infix)
o :: (b -> c) -> (a -> b) -> (a -> c)
o f g = \ a -> f (g a)

feedAndThenRunOver :: Animal -> Animal
feedAndThenRunOver = runOverAnimal . flip feedAnimal 1

-- Eine (geometrische) Figur ist eins der folgenden:
-- - Kreis -ODER-
-- - Quadrat -ODER-
-- - Überlagerung zweier geometrischer Figuren

-- 1. Datentype(en)
-- 2. Funktion, die bei einem Punkt feststellt, 
--    ob er innerhalb oder außerhalb einer geomtrischen Figur liegt

data Point = MkPoint Double Double

point1 = MkPoint 1 1
point2 = MkPoint 3 3
point3 = MkPoint 10 4

data Shape
  = MkCircle {center :: Point, radius :: Double}
  | MkSquare {leftBottom :: Point, sideLength :: Double}
  | MkOverlap {shape1 :: Shape, shape2 :: Shape}

circle1 = MkCircle (MkPoint 2 2) 2.0

square1 = MkSquare (MkPoint 3 3) 4.0

within :: Shape -> Point -> Bool
within (MkCircle (MkPoint centerX centerY) radius) (MkPoint x y) =
  let distanceX = (x - centerX) ^ 2
      distanceY = (y - centerY) ^ 2
      difference = sqrt (distanceX + distanceY)
   in difference <= radius
within (MkSquare (MkPoint squareX squareY) sideLength) (MkPoint x y) =
  let rightTopX = squareX + sideLength
      rightTopY = squareY + sideLength
   in ((x >= squareX) && (x <= rightTopX))
        && ((y >= squareY) && (y <= rightTopY))
within (MkOverlap shape1 shape2) point =
  within shape1 point || within shape2 point


-- Eine Liste ist eins der folgenden:
-- - die leere Liste
-- - eine Cons-Liste aus erstem Element und Rest-Liste

data ListOf a =
    EmptyList
  | Cons a (ListOf a)

-- eingebaut:
-- [] leere Liste
-- first:rest  : = Cons

listSum [] = undefined
listSum (first:rest) = first + (listSum rest)
