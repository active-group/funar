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

instance Eq Pet where
    (==) :: Pet -> Pet -> Bool
    (==) Dog Dog = True
    (==) Cat Cat = True
    (==) Snake Snake = True
    (==) _ _ = False

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

-- function composition
compose :: (b -> c) -> (a -> b) -> (a -> c)
compose f g = \ a -> f (g a)
-- built-in as .

foo :: Animal -> Animal
-- >>> foo dillo1
-- MkDillo {dilloLiveness = Dead, dilloWeight = 15}
foo = runOverAnimal . ((swap feedAnimal) 5)



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

overlap1 = circle1 <> square1

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

-- A list is one of the following:
-- - the empty list []
-- - a cons list of first element and rest list   :

-- 1-element list 5
list1 :: [Integer]
list1 = 5 : []
-- 2-element list 8 5
list2 :: [Integer]
list2 = 8 : (5 : [])

-- 3-element list 8 5 4 
list3 :: [Integer]
list3 = [8, 5, 4]

-- 4-element list 7 8 5 4
list4 :: [Integer]
list4 = 7 : list3


-- built-in as map
listMap :: (a -> b) -> [a] -> [b]
listMap f [] = []
listMap f (a:as) = (f a) : (listMap f as)

integersFrom :: Integer -> [Integer]
integersFrom n = n : integersFrom (n+1)

sieve :: [Integer] -> [Integer]
-- >>> sieve [2..15]
-- [2,3,5,7,11,13]
sieve [] = []
sieve (x:xs) = x : sieve (filter (\y -> mod y x /= 0) xs)

primes :: [Integer]
primes = sieve (integersFrom 2)

data Optional a =
    Null
  | Result a
  deriving (Eq, Show)

-- find the index of an element in a list
-- Eq a: constraint
listIndex :: Eq a => [a] -> a -> Optional Integer

-- >>> listIndex [0, 8, 7, 2, 3] 7
-- Result 2
-- >>> listIndex [Dog, Cat, Cat, Snake, Cat] Snake
-- Result 3
listIndex [] e = Null
listIndex (x:xs) e = 
    if e == x
    then Result 0
    else case listIndex xs e of
           Null -> Null
           Result index -> Result (index+1)

-- >>> :info Eq
-- type Eq :: * -> Constraint
-- type class ... "interface"
-- class Eq a where
--   (==) :: a -> a -> Bool -- method
--   (/=) :: a -> a -> Bool
-- instance Eq Integer -- Defined in ‘GHC.Num.Integer’

-- >>> :info Show
-- type Show :: * -> Constraint
-- class Show a where
--   show :: a -> String

-- >>> :info Ord
-- type Ord :: * -> Constraint
-- class Eq a => Ord a where
--   compare :: a -> a -> Ordering
--   (<) :: a -> a -> Bool
--   (<=) :: a -> a -> Bool
--   (>) :: a -> a -> Bool
--   (>=) :: a -> a -> Bool
--   max :: a -> a -> a
--   min :: a -> a -> a

-- >>> :info Num
-- type Num :: * -> Constraint
-- class Num a where
--   (+) :: a -> a -> a
--   (-) :: a -> a -> a
--   (*) :: a -> a -> a
--   negate :: a -> a
--   abs :: a -> a
--   signum :: a -> a
--   fromInteger :: Integer -> a

-- type (set) + operation(s) + equations/laws
-- a
-- op :: a -> a -> a
-- op a (op b c) == op (op a b) c
-- semigroup

-- a * (b * c) = (a * b) * c  -- associativity

-- (*) :: Integer -> Integer -> Integer
-- overlay :: Image -> Image -> Image
-- shapeOverlap :: Shape -> Shape -> Shape
-- Composite :: Contract -> Contract -> Contract

-- >>> :info Semigroup
-- type Semigroup :: * -> Constraint
-- class Semigroup a where
--   (<>) :: a -> a -> a

instance Semigroup Shape where
    (<>) :: Shape -> Shape -> Shape
    (<>) = MkOverlap

-- instance Semigroup [a] where
--     (<>) = (++)

-- >>> [1,2,3] <> [4,5,6]
-- [1,2,3,4,5,6]

-- semigroup a / <> +
-- identity :: a
-- identity <> x == x <> identity == x
-- 0 + x == x + 0 == x
-- 1 * x == x * 1 == x
-- monoid

-- >>> :info Monoid
-- type Monoid :: * -> Constraint
-- class Semigroup a => Monoid a where
--   mempty :: a

-- instance Monoid [a] where
--    mempty = []

data Additive = MkAdditive Integer
  deriving Show

instance Semigroup Additive where
    (<>) (MkAdditive n1) (MkAdditive n2) = MkAdditive (n1 + n2) 

instance Monoid Additive where
    mempty = MkAdditive 0

-- add the elements of a list
listSum :: [Integer] -> Integer
-- >>> listSum list4
-- 24
listSum [] = 0
listSum (x : xs) = x + listSum xs

listFold e o [] = e
-- listFold e o (x : xs) = o x (listFold e o xs)
listFold e o (x : xs) = x `o` (listFold e o xs)

monoidFold :: Monoid a => [a] -> a
monoidFold = listFold mempty (<>)
-- monoidFold [] = mempty
-- monoidFold (x:xs) = x <> monoidFold xs

-- >>> listCombine (map MkAdditive [1,2,3,4,5])
-- MkAdditive 15

-- instance (Semigroup a, Semigroup b) => Semigroup (a, b) where
--    (<>) :: (a, b) -> (a, b) -> (a, b)
--    (<>) (a1, b1) (a2, b2) = (a1 <> a2, b1 <> b2)

-- instance (Monoid a, Monoid b) => Monoid (a, b) where
--    mempty = (mempty, mempty)

instance Monoid (Optional a) where 
    