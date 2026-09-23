{-# LANGUAGE InstanceSigs #-}
module Intro where

x :: Integer
x = 23 + 42

-- >>> x
-- 65

y :: Integer
y = x * 2

-- Pet is one of the following:
-- - dog OR
-- - cat OR
-- - snake
data Pet = Dog | Cat | Snake
  deriving Show

-- Is a pet cute?
isCute :: Pet -> Bool

-- >>> isCute Dog
-- True
-- >>> isCute Snake
-- False

-- one equation per case
isCute Dog = True
isCute Cat = True
isCute Snake = False

{-
isCute pet = 
    case pet of
        Dog -> True
        Cat -> True
        Snake -> False
-}

-- An animal (on the Texas highway) is one the following:
-- - armadillo  OR
-- - parrot

-- Armadillo has the following attributes:
-- - alive or dead   AND
-- - weight

data Liveness = Alive | Dead
  deriving Show

-- type alias
type Weight = Integer

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
parrot1 = MkParrot "welcome!" 1
parrot2 :: Animal
parrot2 = MkParrot "good riddance!" 2

{-
data Dillo = MkDillo { dilloLiveness :: Liveness,
                       dilloWeight :: Weight }
    deriving Show

dillo1 :: Dillo
dillo1 = MkDillo { dilloLiveness = Alive, dilloWeight = 10 }

dillo2 :: Dillo
dillo2 = MkDillo Alive 8

-- >>> dillo1
-- MkDillo {dilloLiveness = Alive, dilloWeight = 10}

-- >>> dilloLiveness dillo1
-- Alive
-- >>> dilloWeight dillo2
-- 8

-- run over an armadillo
runOverDillo :: Dillo -> Dillo

-- >>> runOverDillo dillo1
-- MkDillo {dilloLiveness = Dead, dilloWeight = 10}

-- runOverDillo dillo =
--    MkDillo { dilloLiveness = Dead, dilloWeight = dilloWeight dillo }
-- runOverDillo dillo = MkDillo Dead (dilloWeight dillo)
--            MkDillo { dilloLiveness = Alive, dilloWeight = 10}
-- runOverDillo (MkDillo { dilloLiveness = l,     dilloWeight = w}) =
--    MkDillo Dead w
-- runOverDillo (MkDillo {dilloWeight = w}) = MkDillo Dead w
-- runOverDillo (MkDillo _ w) = MkDillo Dead w
-- functional update
runOverDillo dillo = dillo { dilloLiveness = Dead }

-}

runOverAnimal :: Animal -> Animal

-- >>> runOverAnimal dillo1
-- MkDillo {dilloLiveness = Dead, dilloWeight = 10}
-- >>> runOverAnimal parrot1
-- MkParrot "" 1

runOverAnimal (MkDillo liveness weight) = MkDillo Dead weight
runOverAnimal (MkParrot sentence weight) = MkParrot "" weight

-- feed animal

-- >>> feedAnimal dillo1 5
-- MkDillo {dilloLiveness = Alive, dilloWeight = 15}
-- >>> feedAnimal dillo2 5
-- MkDillo {dilloLiveness = Dead, dilloWeight = 8}
-- >>> (feedAnimal parrot1) 5
-- MkParrot "welcome!" 6
feedAnimal :: Animal -> Weight -> Animal
feedAnimal (MkDillo liveness weight) amount = 
    case liveness of
       Alive -> MkDillo liveness (weight + amount)
       Dead -> MkDillo liveness weight
-- feedAnimal (MkDillo Alive weight) amount = MkDillo Alive (weight + amount)
-- feedAnimal (MkDillo Dead weight) amount = MkDillo Dead weight
feedAnimal (MkParrot sentence weight) amount =
    MkParrot sentence (weight + amount)

feedAnimal' :: Weight -> Animal -> Animal
feedAnimal' weight animal = feedAnimal animal weight

-- swap :: (Animal -> Weight -> Animal) -> (Weight -> Animal -> Animal)
-- swap f = \ weight -> \ animal -> f animal weight
swap :: (a -> b -> c) -> (b -> a -> c)
-- swap f = \b -> \a -> f a b
swap f b a = f a b
-- built-in as flip

-- >>> swap feedAnimal 5 dillo1
-- MkDillo {dilloLiveness = Alive, dilloWeight = 15}

feedAnimal'' :: (Animal, Weight) -> Animal

-- >>> feedAnimal''(dillo1, 5)
-- MkDillo {dilloLiveness = Alive, dilloWeight = 15}
feedAnimal''(MkDillo liveness weight, amount) =
  case liveness of
    Alive -> MkDillo liveness (weight + amount)
    Dead -> MkDillo liveness weight
feedAnimal''(MkParrot sentence weight, amount) =
  MkParrot sentence (weight + amount)

-- built-in as uncurry
tuplify :: (a -> b -> c) -> ((a, b) -> c)
-- tuplify f = \ (a, b) -> f a b
tuplify f (a, b) = f a b

-- built-in as curry
detuplify :: ((a, b) -> c) -> (a -> b -> c)
-- detuplify f = \ a -> \ b -> f (a, b)
detuplify f a b = f (a, b)

schönfinkeln :: ((a, b) -> c) -> (a -> b -> c)
schönfinkeln f a b = f (a, b)

double :: Integer -> Integer
-- double x = x * 2
-- syntactic sugar:
double = \ x -> x * 2

doublePlus :: Integer -> Integer -> Integer
-- doublePlus x y = x * 2 + y
-- doublePlus = \ x -> \ y -> x * 2 + y
doublePlus x y =
    let d = x * 2
    in d + y

-- A shape is one of the following:
-- - circle
-- - square
-- - an overlay of two shapes

-- 1. write a datatype
-- 2. write a function that, given a point,
--    determines whether it is inside the shape or not

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
