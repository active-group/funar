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

{-
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
-- runOverDillo dillo =
-- Schablone:
--     MkDillo { dilloLiveness = undefined, dilloWeight = undefined }
--   MkDillo {dilloLiveness = Dead, dilloWeight = dilloWeight dillo }
-- runOverDillo (MkDillo { dilloLiveness = l, dilloWeight = w}) = -- Pattern
--    MkDillo { dilloLiveness = Dead, dilloWeight = w }
-- runOverDillo (MkDillo { dilloWeight = w}) =
--  MkDillo { dilloLiveness = Dead, dilloWeight = w }
-- runOverDillo (MkDillo _liveness weight) = MkDillo Dead weight
runOverDillo dillo = dillo { dilloLiveness = Dead } -- "functional update"


-- >>> runOverDillo dillo1
-- MkDillo {dilloLiveness = Dead, dilloWeight = 10}

-- >>> runOverDillo dillo2
-- MkDillo {dilloLiveness = Dead, dilloWeight = 8}
-}


-- Tier auf dem texanischen Highway:
-- - Gürteltier ODER
-- - Klapperschlange

type Thickness = Integer
type Length = Integer

-- algebraischer Datentyp
data Animal
    = MkDillo { dilloLiveness :: Liveness, dilloWeight :: Weight }
    | MkSnake Length Thickness
    deriving Show

dillo1 :: Animal
dillo1 = MkDillo { dilloLiveness = Alive, dilloWeight = 10 }
dillo2 :: Animal
dillo2 = MkDillo Dead 8
snake1 :: Animal
snake1 = MkSnake 300 10

-- Tier überfahren
runOverAnimal :: Animal -> Animal
runOverAnimal (MkDillo _ weight) = MkDillo Dead weight
runOverAnimal (MkSnake length _) = MkSnake length 0

-- >>> runOverAnimal dillo1
-- MkDillo {dilloLiveness = Dead, dilloWeight = 10}
-- >>> runOverAnimal snake1
-- MkSnake 300 0

-- Tier füttern
feedAnimal :: Animal -> Weight -> Animal
feedAnimal dillo@(MkDillo liveness weight) amount = -- Alias-Pattern
    case liveness of
        Alive -> MkDillo liveness (weight + amount)
        Dead -> dillo -- MkDillo liveness weight
feedAnimal (MkSnake length thickness) amount =
    MkSnake length (thickness + amount)

-- swap :: (Animal -> Weight -> Animal) -> (Weight -> (Animal -> Animal))
swap :: (a -> b -> c) -> (b -> a -> c) -- Typvariablen
-- swap f = \ b -> \ a -> f a b
swap f b a = f a b
-- eingebaut flip

feedAnimal' :: (Animal, Weight) -> Animal
feedAnimal'(dillo@(MkDillo liveness weight), amount) =
  -- Alias-Pattern
  case liveness of
    Alive -> MkDillo liveness (weight + amount)
    Dead -> dillo -- MkDillo liveness weight
feedAnimal'(MkSnake length thickness, amount) =
  MkSnake length (thickness + amount)

-- eingebaut als uncurry
tuplify :: (a -> b -> c) -> ((a, b) -> c)
-- tuplify f = \ (a, b) -> f a b
tuplify f (a, b) = f a b

-- >>> tuplify (swap feedAnimal) (5, dillo1)
-- MkDillo {dilloLiveness = Alive, dilloWeight = 15}

-- eingebaut als curry
schönfinkeln :: ((a, b) -> c) -> (a -> b -> c)
-- untuplify f = \ a -> \b -> f (a, b)
schönfinkeln f a b = f (a, b)

-- >>> swap (schönfinkeln feedAnimal') 5 dillo1
-- MkDillo {dilloLiveness = Alive, dilloWeight = 15}

-- >>> feedAnimal'(dillo1, 5)
-- MkDillo {dilloLiveness = Alive, dilloWeight = 15}

-- >>> (swap feedAnimal) 5 dillo1
-- MkDillo {dilloLiveness = Alive, dilloWeight = 15}

feed1 :: Animal -> Animal
feed1 = swap feedAnimal 1

-- >>> feed1 dillo1
-- MkDillo {dilloLiveness = Alive, dilloWeight = 11}
-- >>> feed1 snake1
-- MkSnake 300 11

-- >>> feedAnimal dillo1 5
-- MkDillo {dilloLiveness = Alive, dilloWeight = 15}
-- >>> feedAnimal snake1 7
-- MkSnake 300 17

-- Funktionskomposition
o :: (b -> c) -> (a -> b) -> (a -> c)
o f g = \ a -> f (g a)

-- eingebaut unter dem Namen .
-- Namen aus Sonderzeichen sind Infix 

fff = o runOverAnimal (swap feedAnimal 1)
fff'' = runOverAnimal `o` swap feedAnimal 1
fff' = runOverAnimal . flip feedAnimal 1

-- >>> fff dillo1
-- MkDillo {dilloLiveness = Dead, dilloWeight = 11}

doubleSum :: Integer -> Integer -> Integer
doubleSum x y = (x+y) * 2

-- Eine geometrische Figur ("Shape") ist eins der folgenden:
-- - Kreis -ODER-
-- - Quadrat -ODER-
-- - Überlagerung zweier geometrischer Figuren

-- 1. Datendefinition in Code übersetzen
-- 2. Funktion schreiben, die für einen Punkt ermittelt, ob dieser
--    innerhalb oder außerhalb einer geometrischen Figur liegt

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

data ListOf a
  = EmptyList
  | Cons a (ListOf a)

-- leere Liste: []
-- Cons:        :

list1 :: [Integer]
list1 = 5 : []
list2 = 5 : 2 : []
list3 = [4, 5, 2]

listSum :: [Integer] -> Integer
listSum [] = 0
-- listSum (first:rest) = 
--     first + (listSum rest)
listSum (x:xs) =
    x + (listSum xs)

