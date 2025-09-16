{-# LANGUAGE InstanceSigs #-}
module Intro where

x :: Integer
x = 42

y :: Integer
y = x + 2

double :: Integer -> Integer
-- double = \x -> x * 2 -- "Lambda"
double x = x * 2

plus :: Integer -> Integer -> Integer
plus x y = x + y
-- plus = \ x -> \ y -> x + y
-- plus x = \ y -> x + y

-- >>> (plus 23) 42
-- 65

plus23 :: Integer -> Integer

-- >>> plus23 42
-- 65
plus23 = plus 23

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

{-
data Dillo = MkDillo { dilloLiveness :: Liveness,
                       dilloWeight :: Weight }
  deriving Show

dillo1 :: Dillo
dillo1 = MkDillo { dilloLiveness = Alive, dilloWeight = 10 }

dillo2 :: Dillo
dillo2 = MkDillo Dead 8
-- dillo2 = MkDillo Dead 8

runOverDillo :: Dillo -> Dillo

-- >>> runOverDillo dillo1
-- MkDillo {dilloLiveness = Dead, dilloWeight = 10}
-- >>> runOverDillo dillo2
-- MkDillo {dilloLiveness = Dead, dilloWeight = 8}

-- runOverDillo dillo = MkDillo { dilloLiveness = Dead, dilloWeight = dilloWeight dillo }
-- runOverDillo dillo = MkDillo Dead (dilloWeight dillo)
-- runOverDillo (MkDillo { dilloLiveness = l, dilloWeight = w }) =
--     MkDillo Dead w
-- runOverDillo (MkDillo { dilloWeight = w }) =
--  MkDillo Dead w
-- runOverDillo (MkDillo _ w) = MkDillo Dead w 
-- functional update: Kopie von dillo bis auf { ... }
runOverDillo dillo = dillo { dilloLiveness = Dead }
-} 

-- algebraische Datentypen (ML, OCaml, F#, Haskell, Scala, Swift, ...)

data Animal =
    MkDillo { dilloLiveness :: Liveness, dilloWeight :: Weight }
  | MkParrot String Weight
  deriving Show

dillo1 :: Animal
dillo1 = MkDillo { dilloLiveness = Alive, dilloWeight = 10 }

dillo2 :: Animal
dillo2 = MkDillo Dead 8

parrot1 :: Animal
parrot1 = MkParrot "Hello!" 1
parrot2 :: Animal
parrot2 = MkParrot "Tschüss!" 2

-- Tier überfahren
runOverAnimal :: Animal -> Animal

-- >>> runOverAnimal dillo1
-- MkDillo {dilloLiveness = Dead, dilloWeight = 10}
-- >>> runOverAnimal parrot1
-- MkParrot "" 1
runOverAnimal (MkDillo _liveness weight) = MkDillo Dead weight
runOverAnimal (MkParrot _sentence weight) = MkParrot "" weight

-- Tiere füttern

feedAnimal :: Animal -> (Weight -> Animal)

-- >>> feedAnimal dillo1 5
-- MkDillo {dilloLiveness = Alive, dilloWeight = 15}

feedAnimal dillo@(MkDillo liveness weight) amount = -- Alias-Pattern
  case liveness of
    Alive -> MkDillo liveness (weight+amount)
    Dead -> dillo -- MkDillo liveness weight
feedAnimal (MkParrot sentence weight) amount = MkParrot sentence (weight+amount)

-- Argumente einer Funktion vertauschen
-- swap :: (Animal -> Weight -> Animal) -> (Weight -> Animal -> Animal)
-- (lambda (weight) (lambda (animal) (f animal weight)))
swap :: (a -> b -> c) -> (b -> a -> c) -- Typvariablen
-- swap f =                              \ b -> \ a -> f a b
swap f b a = f a b
-- eingebaut als flip


feedAnimal' :: Weight -> Animal -> Animal
feedAnimal' = swap feedAnimal

-- >>> swap feedAnimal 5 dillo1
-- MkDillo {dilloLiveness = Alive, dilloWeight = 15}

-- >>> feedAnimal' 5 dillo1
-- MkDillo {dilloLiveness = Alive, dilloWeight = 15}

feed1 :: Animal -> Animal
feed1 = feedAnimal' 1

feedDillo1 :: Weight -> Animal
feedDillo1 = feedAnimal dillo1

-- Eine geometrische Figuren ("Shape") ist eins der folgenden:
-- - Kreis
-- - Quadrat
-- - Überlagerung zweier geometrischer Figuren

-- 1. Datentyp für geometrische Figuren
-- 2. Funktion, die feststellt, ob ein Punkt innerhalb einer Figur (oder außerhalb) ist

data Point = MkPoint Double Double

point1 :: Point
point1 = MkPoint 1 1

point2 :: Point
point2 = MkPoint 3 3

point3 :: Point
point3 = MkPoint 10 4

data Shape
  = MkCircle {center :: Point, radius :: Double}
  | MkSquare {leftBottom :: Point, sideLength :: Double}
  | MkOverlap {shape1 :: Shape, shape2 :: Shape} -- Kombinator

circle1 :: Shape
circle1 = MkCircle (MkPoint 2 2) 2.0

square1 :: Shape
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
