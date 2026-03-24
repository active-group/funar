{-# LANGUAGE InstanceSigs #-}
module Intro where

x :: Integer
x = 42

y :: Integer
y = x * 2

-- Zahl verdoppeln
double :: Integer -> Integer


-- >>> double 21
-- 42

-- double = \ x -> x * 2
double x = x * 2 -- syntaktischer Zucker

-- Zahl vervierfachen
quadruple :: Integer -> Integer
-- >>> quadruple 7
-- 28
quadruple x =
    let d = double x
    in double d


doublePlus :: Integer -> Integer -> Integer
-- doublePlus x y = x * 2 + y
doublePlus = \ x -> (\ y -> x * 2 + y)

-- Ein Haustier ist eins der folgenden:
-- - Hund -ODER-
-- - Katze -ODER-
-- - Schlange
data Pet =
    Dog
  | Cat
  | Snake
  deriving Show

instance Eq Pet where
    (==) :: Pet -> Pet -> Bool
    (==) Dog Dog = True
    (==) Cat Cat = True
    (==) Snake Snake = True
    (==) _ _ = False

-- Ist Haustier niedlich?
isCute :: Pet -> Bool

-- >>> isCute Dog
-- True
-- >>> isCute Snake
-- False
{-
isCute pet =
    case pet of
        Dog -> True
        Cat -> True
        Snake -> False
-}
-- 1 Gleichung pro Fall
isCute Dog = True
isCute Cat = True
isCute Snake = False

-- Gürteltier hat:
-- - lebendig -ODER- tot  -UND-
-- - Gewicht
data Liveness =
    Alive | Dead
    deriving Show

type Weight = Integer -- Typsynonym

{-
data Dillo =
    MkDillo { dilloLiveness :: Liveness,
              dilloWeight :: Weight }
    deriving Show

dillo1 :: Dillo
dillo1 = MkDillo { dilloLiveness = Alive, dilloWeight = 10 }

-- totes Gürteltier, 8kg
dillo2 :: Dillo
dillo2 = MkDillo Dead 8 -- syntaktischer Zucker

-- >>> dillo2
-- MkDillo {dilloLiveness = Dead, dilloWeight = 8}

-- >>> dilloLiveness dillo1
-- Alive
-- >>> dilloWeight dillo1
-- 10

-- Gürteltier überfahren
runOverDillo :: Dillo -> Dillo

--- >>> runOverDillo dillo1
-- MkDillo {dilloLiveness = Dead, dilloWeight = 10}

-- runOverDillo dillo = MkDillo { dilloLiveness = Dead, dilloWeight = dilloWeight dillo }
-- runOverDillo dillo = MkDillo Dead (dilloWeight dillo)
-- runOverDillo (MkDillo { dilloLiveness = _l, dilloWeight = w}) = MkDillo Dead w
-- runOverDillo (MkDillo { dilloWeight = w}) = MkDillo Dead w
-- runOverDillo (MkDillo _ weight) = MkDillo Dead weight
-- functional update:
runOverDillo dillo = dillo { dilloLiveness = Dead } -- "Kopie bis auf ..."
-}

-- Ein Tier ist eins der folgenden:
-- - Gürteltier
-- - Papagei
-- algebraischer Datentyp
data Animal =
    MkDillo { dilloLiveness :: Liveness,
              dilloWeight :: Weight }
  | MkParrot String Weight
  deriving Show

dillo1 :: Animal
dillo1 = MkDillo Alive 10
dillo2 :: Animal
dillo2 = MkDillo Dead 8
parrot1 :: Animal
parrot1 = MkParrot "Welcome!" 1


runOverAnimal :: Animal -> Animal

-- >>> runOverAnimal dillo1
-- MkDillo {dilloLiveness = Dead, dilloWeight = 10}
-- >>> runOverAnimal parrot1
-- MkParrot "" 1
runOverAnimal dillo@MkDillo {} = dillo { dilloLiveness = Dead }  -- Alias-Pattern
--- runOverAnimal (MkDillo _ weight) = MkDillo Dead weight
runOverAnimal (MkParrot sentence weight) = MkParrot "" weight

-- Tier füttern
feedAnimal :: Animal -> Weight -> Animal
feedAnimal (MkDillo liveness weight) food =
    case liveness of
        Alive -> MkDillo liveness (weight + food)
        Dead -> MkDillo liveness weight
feedAnimal (MkParrot sentence weight) food =
    MkParrot sentence (weight + food)

-- >>> feedAnimal'(dillo1, 5)
-- MkDillo {dilloLiveness = Alive, dilloWeight = 15}

-- Tupel
-- >>> (dillo1, 5)
-- (MkDillo {dilloLiveness = Alive, dilloWeight = 10},5)


feedAnimal' :: (Animal, Weight) -> Animal
feedAnimal'(MkDillo liveness weight, food) =
    case liveness of
        Alive -> MkDillo liveness (weight + food)
        Dead -> MkDillo liveness weight
feedAnimal'(MkParrot sentence weight, food) =
    MkParrot sentence (weight + food)

-- Haskell Curry -> "to curry"
-- Moses Schönfinkel

-- eingebaut als uncurry
-- tuplify :: (Animal -> Weight -> Animal) -> ((Animal, Weight) -> Animal)
entschönfinkeln :: (a -> b -> c) -> ((a, b) -> c) -- Typvariablen
-- tuplify f = \ (animal, weight) -> f animal weight
-- tuplify f = \(a, b) -> f a b
entschönfinkeln f (a, b) = f a b

-- eingebaut als curry
schönfinkeln :: ((a, b) -> c) -> (a -> b -> c)
-- untuplify f = \ a -> \ b -> f (a, b)
schönfinkeln f a b = f (a, b)

-- eingebaut als flip
swap :: (a -> b -> c) -> (b -> a -> c)
swap    f =              \b -> \a -> f a b 

-- Funktionskomposition
-- eingebaut als . (Infix)
o :: (b -> c) -> (a -> b) -> (a -> c)
o    f           g =         \a -> f (g a)

(|>) :: (a -> b) -> (b -> c) -> (a -> c)
--(|>) g f = \a -> f (g a) 
(|>) = flip (.)

foo :: Animal -> Animal
foo = o runOverAnimal (swap feedAnimal 5)
foo' = runOverAnimal . swap feedAnimal 5
foo'' = runOverAnimal `o` (swap feedAnimal 5)
foo''' = (.) runOverAnimal (swap feedAnimal 5)
foo'''' = swap feedAnimal 5 |> runOverAnimal

-- >>> tuplify feedAnimal (dillo1, 5)
-- MkDillo {dilloLiveness = Alive, dilloWeight = 15}

-- Eine geometrische Figur ("Shape") ist eins der folgenden:
-- - Kreis
-- - Quadrat
-- - Überlagerung zweier geometrischer Figuren

-- Aufgabe:
-- 1. Datentyp zu dieser Datendefinition
-- 2. Funktion, die feststellt, ob ein Punkt innerhalb einer
--    geometrischen Figur ist

data Point = MkPoint Double Double
  deriving (Show, Eq)

point1 :: Point
point1 = MkPoint 1 1

point2 :: Point
point2 = MkPoint 3 3

point3 :: Point
point3 = MkPoint 10 4

-- algebraischer Datentyp
data Shape
  = MkCircle {center :: Point, radius :: Double}
  | MkSquare {leftBottom :: Point, sideLength :: Double}
  | MkOverlap {shape1 :: Shape, shape2 :: Shape} -- Kombinator
  deriving (Show)

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

data ListOf a =
    Empty
  | Cons a (ListOf a)

-- leere Liste: []
-- Cons:        :

-- 1elementige Liste: 5
list1 :: [Integer]
list1 = 5 : []
list2 :: [Integer]
list2 = 2 : (5 : [])
list3 :: [Integer]
list3 = [7, 2, 5]  -- syntaktischer Zucker
list4 :: [Integer]
list4 = 4 : list3

-- Liste aufsummieren
listSum :: [Integer] -> Integer

-- >>> listSum list4
-- 18
listSum [] = 0
listSum (first:rest) =
    first + (listSum rest)

data Optional a =
    None
  | Some a
  deriving (Eq, Show) -- der Compiler macht eine Standard-Instanz

-- Index eines Listenelements
-- Eq a: Constraint

-- Eq: Typklasse ~ "Interface"
-- Implementierung: Instanz / "instance"

-- >>> :info Eq
-- type Eq :: * -> Constraint
-- class Eq a where
--   (==) :: a -> a -> Bool
--   (/=) :: a -> a -> Bool
-- instance Eq Integer -- Defined in ‘GHC.Num.Integer’
-- instance Eq Bool -- Defined in ‘GHC.Classes’

-- >>> :info Show
-- type Show :: * -> Constraint
-- class Show a where
--   show :: a -> String


listIndex :: Eq a => a -> [a] -> Optional Integer

-- >>> listIndex 2 [1, 4, 3, 2, 5]
-- Some 3
-- >>> listIndex Snake [Dog, Cat, Dog, Snake, Dog, Cat]
-- Some 3

listIndex a [] = None
listIndex a (x:xs) =
    if a == x
    then Some 0
    else
        case listIndex a xs of
            None -> None
            Some index -> Some (index + 1)
