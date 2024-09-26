{-# LANGUAGE InstanceSigs #-}
module Intro where

import Prelude hiding (Semigroup, Monoid)
import Text.Read (readMaybe)

x :: Integer
x = 23

-- >>> double 12
-- 24

double :: Integer -> Integer
double x =  -- Abseitsregel
 x * 2

double' :: Integer -> Integer
double' = \ x -> x * 2 

foo :: Integer -> Integer -> Integer
foo = \ x -> \ y ->
    let z = x + y
        a = z + (2*y)
    in (x + y + z) * a

-- Gürteltier hat folgende Eigenschaften:
-- - Lebendigkeit -UND-
-- - Gewicht

-- Lebendigkeit ist eins der folgenden:
-- - Lebendig
-- - Tot

data Liveness = Alive | Dead
  deriving (Show, Eq)

{-
instance Eq Liveness where
    (==) :: Liveness -> Liveness -> Bool
    (==) Alive Alive = True
    (==) Dead Dead = True
    (==) _ _ = False
-}

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

-- >>> runOverDillo dillo1
-- MkDillo {dilloLiveness = Dead, dilloWeight = 10}

runOverDillo :: Dillo -> Dillo
-- runOverDillo dillo =
--    MkDillo { dilloLiveness = Dead, dilloWeight = dilloWeight dillo }
-- runOverDillo (MkDillo { dilloWeight = w}) =
--    MkDillo { dilloLiveness = Dead, dilloWeight = w }
runOverDillo (MkDillo _liveness weight) =
    MkDillo Dead weight
-- runOverDillo dillo = dillo { dilloLiveness = Dead } -- functional update

-}

-- data Dillo = MkDillo Liveness Weight

-- algebraischer Datentyp
data Animal =
    MkDillo Liveness Weight
  | MkSnake { snakeLength :: Integer, snakeThickness :: Integer }
  deriving Show

instance Eq Animal where
  (==) :: Animal -> Animal -> Bool
  (==) (MkDillo liveness1 weight1) (MkDillo liveness2 weight2) =
    (liveness1 == liveness2) && (weight1 == weight2)
  (==) (MkSnake length1 thickness1) (MkSnake length2 thickness2) =
    (length1 == length2) && (thickness1 == thickness2)
  (==) (MkDillo _ _) (MkSnake _ _) = False
  (==) (MkSnake _ _) (MkDillo _ _) = False

dillo1 = MkDillo Alive 10
dillo2 = MkDillo Dead 8
snake1 = MkSnake 200 5
snake2 = MkSnake 500 20

-- >>> runOverAnimal dillo1 
-- MkDillo {dilloLiveness = Dead, dilloWeight = 10}
-- >>> runOverAnimal snake1
-- MkSnake {snakeLength = 200, snakeThickness = 0}

runOverAnimal :: Animal -> Animal
-- 1 Gleichung pro Fall
-- Schablone:
-- runOverAnimal (MkDillo liveness weight) = undefined
-- runOverAnimal (MkSnake length thickness) = undefined

runOverAnimal (MkDillo _liveness weight) =
    MkDillo Dead weight
runOverAnimal (MkSnake length _thickness) =
    MkSnake length 0


feedAnimal :: Animal -> (Weight -> Animal)
{-
feedAnimal (MkDillo Alive weight) amount =
  MkDillo Alive (weight+amount)
feedAnimal dillo@(MkDillo Dead _) amount =
      dillo -- MkDillo liveness weight
-}
feedAnimal dillo@(MkDillo liveness weight) amount = -- Alias-Pattern
    case liveness of
      Alive -> MkDillo Alive (weight+amount)
      Dead -> dillo -- MkDillo liveness weight
feedAnimal (MkSnake length thickness) amount =
    MkSnake length (thickness+amount)

-- swap :: (Animal -> Weight -> Animal) -> (Weight -> Animal -> Animal)
-- Typvariablen: Kleinbuchstaben
swap :: (a -> b -> c) -> (b -> a -> c)
--swap f = \ b -> \ a -> f a b
swap f b a = f a b

feedAnimal' :: (Animal, Weight) -> Animal
feedAnimal'(dillo@(MkDillo liveness weight), amount) =
  -- Alias-Pattern
  case liveness of
    Alive -> MkDillo Alive (weight + amount)
    Dead -> dillo -- MkDillo liveness weight
feedAnimal'(MkSnake length thickness, amount) =
  MkSnake length (thickness + amount)

-- eingebaut als uncurry
entschönfinkeln :: (a -> b -> c) -> ((a, b) -> c)
--tuplify f = \ (a, b) -> f a b
entschönfinkeln f (a, b) = f a b

-- Haskell Curry
-- Moses Schönfinkel

-- eingebaut als curry
schönfinkeln :: ((a, b) -> c) -> (a -> b -> c)
schönfinkeln f a b =  f (a, b)

-- Funktionskomposition, eingebaut als .
o :: (b -> c) -> (a -> b) -> (a -> c)
o f g = \ a -> f (g a)

-- Eine geometrische Figur ("Shape") ist eins der folgenden:
-- - ein Kreis
-- - Quadrat
-- - eine Überlagerung zweiter geometrischer Figuren

-- 1. Datentyp dafür
-- 2. Funktion, die für einen Punkt feststellt, ob dieser
--    innerhalb oder außerhalb einer geometrischen Figur
--    liegt

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

data ListOf a =
    Empty 
  | Cons a (ListOf a)
  deriving Show

-- leere Liste: []
-- Cons: :

list1 :: [Integer]
list1 = 5 : []

list2 :: [Integer]
list2 = 2 : 5 : []

list3 :: [Integer]
list3 = 7 : list2

list4 :: [Integer]
list4 = [4, 7, 2, 5]

-- >>> listSum list4
-- 18

listSum :: [Integer] -> Integer
listSum [] = 0
listSum (x:xs) =
    x + listSum xs

listMap :: (a -> b) -> [a] -> [b]
listMap f [] = []
listMap f (x:xs) = (f x) : (listMap f xs)

xx = 5
-- >>> extract (\ x -> x `mod` 2 == 0) list4
-- [4,2]

extract :: (a -> Bool) -> [a] -> [a]
extract p [] = []
extract p (x:xs) =
    if p x
    then x : extract p xs
    else extract p xs

natsFrom :: Integer -> [Integer]
natsFrom n = n : natsFrom (n+1)

-- alle Vielfachen einer Zahl entfernen
strikeMultiples :: Integer -> [Integer] -> [Integer]
strikeMultiples n list =
    filter (\ x -> mod x n /= 0) list

-- die erste Zahl ist eine Primzahl
sieve :: [Integer] -> [Integer]
sieve [] = []
sieve (x:xs) = x:(sieve (strikeMultiples x xs))

primes :: [Integer]
primes = sieve (natsFrom 2)

data Optional a =
    Null 
  | Result a
  deriving Show

-- >>> listIndex 5 [1,5,7,9]
-- Result 1

-- >>> listIndex dillo1 [snake1, snake2, dillo1, dillo2]
-- No instance for (Eq Animal) arising from a use of `listIndex'
-- In the expression:
--   listIndex dillo1 [snake1, snake2, dillo1, dillo2]
-- In an equation for `it_a8tcI':
--     it_a8tcI = listIndex dillo1 [snake1, snake2, dillo1, dillo2]

-- Eq a: Constraint => Gleichheit existiert für a

-- Index eines Elements in einer Liste finden
listIndex :: Eq a => a -> [a] -> Optional Integer
listIndex x [] = Null
listIndex x (y:ys) =
    if x == y
    then Result 0
    else
        optionalMap (+1) -- (\ index -> index + 1) 
                    (listIndex x ys)
        {-
        case listIndex x ys of
            Null -> Null
            Result index -> Result (index+1)
        -}

-- listMap  :: (a -> b) -> List     a -> List     b
optionalMap :: (a -> b) -> Optional a -> Optional b
optionalMap f Null = Null
optionalMap f (Result a) = Result (f a)

-- >>> :info Functor
-- type Functor :: (* -> *) -> Constraint
-- class Functor f where
--   fmap :: (a -> b) -> f a -> f b

instance Functor Optional where
    fmap = optionalMap

-- >>> :info Applicative
-- type Applicative :: (* -> *) -> Constraint
-- class Functor f => Applicative f where
--   pure :: a -> f a
--   (<*>) :: f (a -> b) -> f a -> f b

instance Applicative Optional where
    pure :: a -> Optional a
    pure = Result
    (<*>) :: Optional (a -> b) -> Optional a -> Optional b
    (<*>) Null Null = Null
    (<*>) (Result f) Null = Null
    (<*>) Null (Result a) = Null
    (<*>) (Result f) (Result a) = Result (f a)


-- (<$>) = fmap

-- optionalMap2 :: (a -> b -> c) -> Optional a -> Optional b -> Optional c
optionalMap2 :: Applicative f => (a -> b -> c) -> (f a -> f b -> f c)
-- optionalMap2 f2 oa ob = fmap f2 oa <*> ob
optionalMap2 f2 oa ob = f2 <$> oa <*> ob

fmap3 f3 aa ab ac = f3 <$> aa <*> ab <*> ac

-- fmap  ::      (a ->   b) -> f a -> f b
-- (<*>) ::    f (a ->   b) -> f a -> f b
-- flip (>>=) :: (a -> f b) -> f a -> f b
-- (>>=) ::    f a -> (a -> f b) -> f b

instance Monad Optional where
    return = Result
    (>>=) :: Optional a -> (a -> Optional b) -> Optional b
    (>>=) Null _next = Null
    (>>=) (Result a) next = next a

incOptional :: Optional Integer -> Optional Integer
incOptional = fmap (+1)


plusOptional :: Optional Integer -> Optional Integer -> Optional Integer
plusOptional = optionalMap2 (+)
{-
plusOptional o1 o2 =
    do n1 <- o1
       n2 <- o2
       return (n1+n2)
-}

-- fmap id == id
-- fmap f . fmap g == fmap (f . g)

data Validation a =
    Valid a
  | Invalid [String] -- Fehlermeldungen
  deriving Show

instance Functor Validation where
  fmap f (Valid a) = Valid (f a)
  fmap f (Invalid errors) = Invalid errors

instance Applicative Validation where
  pure = Valid
  (<*>) (Valid fa) (Valid a) = Valid (fa a)
  (<*>) (Valid _) (Invalid errors) = Invalid errors
  (<*>) (Invalid errors) (Valid _) = Invalid errors
  (<*>) (Invalid errors1) (Invalid errors2) = Invalid (errors1 ++ errors2)

data User = MkUser Name Age
  deriving Show

makeUser :: String -> Integer -> Validation User
makeUser name n =
    MkUser <$> (makeName name) <*> (makeAge n) 

newtype Name = MkName String
    deriving Show

makeName :: String -> Validation Name
makeName name =
    if length name < 100
    then Valid (MkName name)
    else Invalid ["name too long"]

newtype Age = MkAge Integer
    deriving Show

makeAge :: Integer -> Validation Age
makeAge n =
    if (n >= 18) && (n <= 120)
    then Valid (MkAge n)
    else Invalid ["not an appropriate age"]




-- >>> :info Eq
-- type Eq :: * -> Constraint
-- class Eq a where
--   (==) :: a -> a -> Bool
--   (/=) :: a -> a -> Bool

-- Typklasse ... Interface

-- >>> :info Show
-- type Show :: * -> Constraint
-- class Show a where
--   showsPrec :: Int -> a -> ShowS
--   show :: a -> String
--   showList :: [a] -> ShowS

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

-- Halbgruppe:
-- - Typ t
-- - Operation op :: t -> t -> t
-- - Assoziativität: op x (op y z) == op (op x y) z

class Semigroup t where -- "t bildet eine Halbgruppe"
  op :: t -> t -> t

instance Semigroup [a] where
    op :: [a] -> [a] -> [a]
    op = (++)

-- Monoid: Halbgruppe mit neutralem Element
-- op x neutral == op neutral x == x

class Semigroup t => Monoid t where
    neutral :: t

instance Monoid [a] where
    neutral :: [a]
    neutral = []

listOp :: Monoid a => [a] -> a
listOp [] = neutral
listOp (x:xs) = op x (listOp xs)

instance (Semigroup x, Semigroup y) => Semigroup (x, y) where
    op :: (x, y) -> (x, y) -> (x, y)
    op (x1, y1) (x2, y2) =
        (op x1 x2, op y1 y2)

-- Übung

instance Semigroup a => Semigroup (Optional a) where
    op Null x = x
    op x Null = x
    op (Result a) (Result a') = Result (op a a')

instance Semigroup a => Monoid (Optional a) where
    neutral = Null
