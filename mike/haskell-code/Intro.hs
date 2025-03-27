{-# LANGUAGE InstanceSigs #-}
module Intro where

import Prelude hiding (Semigroup, Monoid)

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

instance Eq Pet where
    (==) :: Pet -> Pet -> Bool
    (==) Dog Dog = True
    (==) Cat Cat = True
    (==) Snake Snake = True
    (==) _ _ = False


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
  deriving (Show, Eq)

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

data Optional a 
  = Null
  | Result a
  deriving Show

listMap ::     (a -> b) ->        [a] ->         [b]
listMap f [] = []
listMap f (x:xs) = f x  : (listMap f xs)

optionalMap :: (a -> b) -> Optional a -> Optional b
optionalMap f Null = Null
optionalMap f (Result a) = Result (f a)

-- >>> :info Functor
-- type Functor :: (* -> *) -> Constraint
-- class Functor f where
--   fmap :: (a -> b) -> f a -> f b

instance Functor Optional where
    fmap = optionalMap

-- eingebaut als:
-- data Maybe a = Nothing | Just a

-- Index eines Elements in einer Liste
-- Eq a: Constraint, "Werte des Typs a sind vergleichbar"
listIndex :: Eq a => [a] -> a -> Optional Integer
listIndex [] a = Null
listIndex (x:xs) a =
    if x == a
    then Result 0 
    else 
        -- optionalMap (\ x -> x + 1) (listIndex xs a)
        fmap (1+) (listIndex xs a)
{-        case listIndex xs a of
            Null -> Null
            Result index -> Result (index + 1)
-}

-- >>> listIndex [1, 0, 2, 4, 7] 4
-- Result 3

-- >>> listIndex [Cat, Cat, Snake, Snake, Dog, Cat] Dog
-- Result 4

-- Typklasse:

-- >>> :info Eq
-- type Eq :: * -> Constraint
-- class Eq a where
--   (==) :: a -> a -> Bool
--   (/=) :: a -> a -> Bool
--   {-# MINIMAL (==) | (/=) #-}
--   	-- Defined in ‘GHC.Classes’
-- instance (Eq a, Eq b) => Eq (Either a b)
--   -- Defined in ‘Data.Either’
-- instance Eq a => Eq (Maybe a) -- Defined in ‘GHC.Maybe’
-- instance Eq Integer -- Defined in ‘GHC.Num.Integer’
-- instance Eq Bool -- Defined in ‘GHC.Classes’
-- instance Eq Char -- Defined in ‘GHC.Classes’
-- instance Eq Double -- Defined in ‘GHC.Classes’
-- instance Eq Float -- Defined in ‘GHC.Classes’
-- instance Eq Int -- Defined in ‘GHC.Classes’
-- instance Eq a => Eq [a] -- Defined in ‘GHC.Classes’
-- instance Eq Ordering -- Defined in ‘GHC.Classes’
-- instance Eq a => Eq (Solo a) -- Defined in ‘GHC.Classes’
-- instance (Eq a, Eq b, Eq c, Eq d, Eq e, Eq f, Eq g, Eq h, Eq i,
--           Eq j) =>
--          Eq (a, b, c, d, e, f, g, h, i, j)
--   -- Defined in ‘GHC.Classes’
-- instance (Eq a, Eq b, Eq c, Eq d, Eq e, Eq f, Eq g, Eq h, Eq i,
--           Eq j, Eq k) =>
--          Eq (a, b, c, d, e, f, g, h, i, j, k)
--   -- Defined in ‘GHC.Classes’
-- instance (Eq a, Eq b, Eq c, Eq d, Eq e, Eq f, Eq g, Eq h, Eq i,
--           Eq j, Eq k, Eq l) =>
--          Eq (a, b, c, d, e, f, g, h, i, j, k, l)
--   -- Defined in ‘GHC.Classes’
-- instance (Eq a, Eq b, Eq c, Eq d, Eq e, Eq f, Eq g, Eq h, Eq i,
--           Eq j, Eq k, Eq l, Eq m) =>
--          Eq (a, b, c, d, e, f, g, h, i, j, k, l, m)
--   -- Defined in ‘GHC.Classes’
-- instance (Eq a, Eq b, Eq c, Eq d, Eq e, Eq f, Eq g, Eq h, Eq i,
--           Eq j, Eq k, Eq l, Eq m, Eq n) =>
--          Eq (a, b, c, d, e, f, g, h, i, j, k, l, m, n)
--   -- Defined in ‘GHC.Classes’
-- instance (Eq a, Eq b, Eq c, Eq d, Eq e, Eq f, Eq g, Eq h, Eq i,
--           Eq j, Eq k, Eq l, Eq m, Eq n, Eq o) =>
--          Eq (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o)
--   -- Defined in ‘GHC.Classes’
-- instance (Eq a, Eq b) => Eq (a, b) -- Defined in ‘GHC.Classes’
-- instance (Eq a, Eq b, Eq c) => Eq (a, b, c)
--   -- Defined in ‘GHC.Classes’
-- instance (Eq a, Eq b, Eq c, Eq d) => Eq (a, b, c, d)
--   -- Defined in ‘GHC.Classes’
-- instance (Eq a, Eq b, Eq c, Eq d, Eq e) => Eq (a, b, c, d, e)
--   -- Defined in ‘GHC.Classes’
-- instance (Eq a, Eq b, Eq c, Eq d, Eq e, Eq f) =>
--          Eq (a, b, c, d, e, f)
--   -- Defined in ‘GHC.Classes’
-- instance (Eq a, Eq b, Eq c, Eq d, Eq e, Eq f, Eq g) =>
--          Eq (a, b, c, d, e, f, g)
--   -- Defined in ‘GHC.Classes’
-- instance (Eq a, Eq b, Eq c, Eq d, Eq e, Eq f, Eq g, Eq h) =>
--          Eq (a, b, c, d, e, f, g, h)
--   -- Defined in ‘GHC.Classes’
-- instance (Eq a, Eq b, Eq c, Eq d, Eq e, Eq f, Eq g, Eq h, Eq i) =>
--          Eq (a, b, c, d, e, f, g, h, i)
--   -- Defined in ‘GHC.Classes’
-- instance Eq () -- Defined in ‘GHC.Classes’
-- instance Eq Word -- Defined in ‘GHC.Classes’

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
--   {-# MINIMAL compare | (<=) #-}
--   	-- Defined in ‘GHC.Classes’

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

-- algebraische Struktur:
-- - Menge/Typ
-- - Operation(en)
-- - Gleichungen

-- Halbgruppe:
-- Typ a
-- Operation
-- mörtsch :: a -> a -> a
class Semigroup a where
    -- mörtsch a (mörtsch b c) == mörtsch (mörtsch a b) c
    mörtsch :: a -> a -> a

instance Semigroup Integer where
    mörtsch :: Integer -> Integer -> Integer
    mörtsch = (+)

instance Semigroup [a] where
    mörtsch :: [a] -> [a] -> [a]
    mörtsch = (++)

-- >>> mörtsch [1, 2, 3] [4, 5, 6] 
-- [1,2,3,4,5,6]

instance (Semigroup a, Semigroup b) => Semigroup (a, b) where
    mörtsch (a1, b1) (a2, b2) = (mörtsch a1 a2, mörtsch b1 b2)

-- >>> mörtsch ([1,2,3], 5 :: Integer) ([4,5,6], 7)
-- ([1,2,3,4,5,6],12)

class Semigroup a => Monoid a where
    -- mörtsch neutral x == mörtsch x neutral == x
    neutral :: a

instance Monoid Integer where
    neutral :: Integer
    neutral = 0

instance Monoid [a] where
    neutral :: [a]
    neutral = []

instance (Monoid a, Monoid b) => Monoid (a, b) where
    neutral = (neutral, neutral)

instance (Semigroup a) => Semigroup (Optional a) where
  mörtsch Null Null = Null
  mörtsch (Result a1) Null = Result a1
  mörtsch Null (Result a2) = Result a2
  mörtsch (Result a1) (Result a2) = Result (mörtsch a1 a2)

-- >>> mörtsch (Result 5) (Result 12)
-- Result 17

-- >>> mörtsch (Result Dog) (Result Cat)
-- No instance for `Semigroup Pet' arising from a use of `mörtsch'
-- In the expression: mörtsch (Result Dog) (Result Cat)
-- In an equation for `it_ahugC':
--     it_ahugC = mörtsch (Result Dog) (Result Cat)

instance Semigroup a => Monoid (Optional a) where
    neutral = Null

instance Semigroup b => Semigroup (a -> b) where
    mörtsch f g = \ a -> mörtsch (f a) (g a)

-- >>> (mörtsch double quadruple) 5
-- 110

-- E-Mail-Adresse, Alter
data User = MkUser String Integer
  deriving Show

validateEMail :: String -> Optional String
validateEMail email =
    case listIndex email '@' of 
        Result _ -> Result email
        Null -> Null

validateAge :: Integer -> Optional Integer
validateAge n =
    if n >= 0 && n <= 120
    then Result n
    else Null
