{-# LANGUAGE InstanceSigs #-}
module Intro where

import Prelude hiding (Semigroup, Monoid)
import Data.Text.Internal.Encoding.Utf32 (validate)


x :: Integer
x = 42

double :: Integer -> Integer
-- double x = x * 2
double = \ x -> x * 2 -- \ lambda

-- >>> double 42
-- 84

foo :: Integer -> Integer
foo x =
    let y = x * 2
        z = y + 1
    in x + y + z

-- >>> foo 3
-- 16

-- Haustier ist eins der folgenden:
-- - Hund -ODER-
-- - Katze -ODER-
-- - Schlange
-- Fallunterscheidung/Aufzählung

data Pet =
    Dog 
  | Cat
  | Snake
  deriving (Show, Ord)

-- Ist Haustier niedlich?
isCute :: Pet -> Bool
-- eine Gleichung pro Fall ... Pattern-Matching
isCute Dog = True
isCute Cat = True
isCute Snake = False

-- >>> isCute Dog
-- True

-- >>> isCute Cat
-- True

-- >>> isCute Snake
-- False

-- Gürteltier hat folgende Eigenschaften:
-- - lebendig -ODER- tot
-- - Gewicht

data Liveness =
    Alive 
  | Dead
  deriving (Show, Eq)  -- der Compiler soll eine Instanz von Eq herleiten

-- Typalias
type Weight = Integer

{-
data Dillo =
    MkDillo { dilloLiveness :: Liveness,
              dilloWeight :: Weight }
    deriving Show

-- lebendiges Gürteltier, 10kg
dillo1 :: Dillo
dillo1 = MkDillo { dilloLiveness = Alive, dilloWeight = 10}

-- totes Gürteltier, 8kg
dillo2 :: Dillo
dillo2 = MkDillo Dead 8

runOverDillo :: Dillo -> Dillo
-- runOverDillo dillo = MkDillo { dilloLiveness = Dead, dilloWeight = dilloWeight dillo }
-- runOverDillo (MkDillo { dilloLiveness = l, dilloWeight = w}) = MkDillo { dilloLiveness = Dead, dilloWeight = w}
-- runOverDillo (MkDillo { dilloWeight = w }) = MkDillo Dead w
-- runOverDillo (MkDillo _ w) = MkDillo Dead w
-- "functional update":
runOverDillo dillo = dillo { dilloLiveness = Dead } -- Kopie von dillo, bis auf ...

-- >>> runOverDillo dillo1
-- MkDillo {dilloLiveness = Dead, dilloWeight = 10}
-}

-- Tier (auf dem texanischen Highway) ist eins der folgenden:
-- - Gürteltier -ODER-
-- - Papagei
-- gemischte Daten, in Haskell alle in einen Datentyp

-- algebraischer Datentyp

data Animal =
    MkDillo { dilloLiveness :: Liveness,
              dilloWeight :: Weight}
  | MkParrot String Weight
  deriving Show

dillo1 :: Animal
dillo1 = MkDillo { dilloLiveness = Alive, dilloWeight = 10 }
dillo2 :: Animal
dillo2 = MkDillo Dead 8

parrot1 :: Animal
parrot1 = MkParrot "hello!" 1
parrot2 :: Animal
parrot2 = MkParrot "goodbye!" 2

-- Tier überfahren
runOverAnimal :: Animal -> Animal
runOverAnimal (MkDillo liveness weight) = MkDillo Dead weight
runOverAnimal (MkParrot sentence weight) = MkParrot "" weight

-- >>> runOverAnimal dillo1
-- MkDillo {dilloLiveness = Dead, dilloWeight = 10}

-- >>> runOverAnimal parrot1
-- MkParrot "" 1

-- Tier füttern
feedAnimal :: Animal -> (Weight -> Animal)
feedAnimal dillo@(MkDillo liveness weight) amount = -- Alias-Pattern
    case liveness of
        Alive -> MkDillo Alive (weight + amount)
        Dead -> dillo -- MkDillo liveness weight
feedAnimal (MkParrot sentence weight) amount = MkParrot sentence (weight + amount)

-- Besser wäre vielleicht feedAnimal :: Weight -> (Animal -> Animal)

-- eingebaut als flip
-- swap :: (Animal -> Weight -> Animal) -> (Weight -> Animal -> Animal)
swap :: (a -> b -> c) -> (b -> a -> c)
-- swap f = \ b -> \ a -> f a b
swap f b a = f a b

-- Haskell Curry
-- Moses Schönfinkel

-- eingebaut als uncurry
tuplify :: (a -> b -> c) -> ((a, b) -> c)
-- tuplify f = \ (a, b) -> f a b
tuplify f (a, b) = f a b

-- >>> (tuplify feedAnimal) (dillo1, 5)
-- MkDillo {dilloLiveness = Alive, dilloWeight = 15}

-- eingebaut als curry
untuplify :: ((a, b) -> c) -> (a -> b -> c)
-- untuplify f = \ a -> \ b -> f (a, b)
untuplify f a b = f (a, b)

-- Funktionskomposition, eingebaut als .
o :: (b -> c) -> (a -> b) -> (a -> c)
o f g = \ a -> f (g a)

-- >>> (runOverAnimal . flip feedAnimal 1) dillo1
-- MkDillo {dilloLiveness = Dead, dilloWeight = 11}

-- >>> untuplify feedAnimal' dillo1 5
-- MkDillo {dilloLiveness = Alive, dilloWeight = 15}

(|>) :: a -> (a -> b) -> b
(|>) a f = f a

-- >>> dillo1 |> flip feedAnimal 1 |> runOverAnimal
-- MkDillo {dilloLiveness = Dead, dilloWeight = 11}

feedAnimalR :: Weight -> Animal -> Animal
feedAnimalR = swap feedAnimal

-- >>> feedAnimal dillo1 5
-- MkDillo {dilloLiveness = Alive, dilloWeight = 15}

-- >>> feedAnimal parrot1 5
-- MkParrot "hello!" 6

-- >>> feedAnimal dillo2 5
-- MkDillo {dilloLiveness = Dead, dilloWeight = 8}

-- Tupel
feedAnimal' :: (Animal, Weight) -> Animal
feedAnimal' (dillo@(MkDillo liveness weight), amount) =
  -- Alias-Pattern
  case liveness of
    Alive -> MkDillo Alive (weight + amount)
    Dead -> dillo -- MkDillo liveness weight
feedAnimal' (MkParrot sentence weight, amount) = MkParrot sentence (weight + amount)

-- Eine geometrische Figur ("Shape") ist eins der folgenden:
-- - ein Kreis
-- - ein Quadrat
-- - eine Überlagerung zweier geometrischer Figuren

-- Aufgabe:
-- 1. Datentyp
-- 2. Funktion, die ermittelt, ob ein Punkt innerhalb oder außerhalb einer geometrischen Figur liegt

data Point = MkPoint Double Double
  deriving (Eq, Show)

point1 = MkPoint 1 1

point2 = MkPoint 3 3

point3 = MkPoint 10 4

data Shape
  = MkCircle {center :: Point, radius :: Double}
  | MkSquare {leftBottom :: Point, sideLength :: Double}
  | MkOverlap {shape1 :: Shape, shape2 :: Shape}
  deriving (Eq, Show)

circle1 = MkCircle (MkPoint 2 2) 2.0

circle2 = MkCircle (MkPoint 4 4) 4.0

square1 = MkSquare (MkPoint 3 3) 4.0

-- Denotation

within :: Shape -> (Point -> Bool)
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
-- - eine Cons-Liste aus erstem Element und Rest
data ListOf a = -- Typvariable
    Empty 
  | Cons a (ListOf a)

-- 2elementige Liste
loi2 :: ListOf Integer
loi2 = Cons 5 (Cons 7 Empty)

-- die leere Liste: []
-- Cons:            :   (Sonderzeichen sind Infix-Funktionen)

list1 :: [Integer] -- eine Liste aus Integers
list1 = 5 : []

list2 :: [Integer]
list2 = 2 : (5 : [])

list3 :: [Integer]
list3 = [7,2,5]

list4 :: [Integer]
list4 = 8:list3

listSum :: [Integer] -> Integer
listSum [] = 0
listSum (first : rest) =  first + listSum rest

-- >>> listSum list4
-- 22

-- (: list-fold (%r (%a %r -> %r) (list-of %a) -> %r))

listFold :: r -> (a -> r -> r) -> [a] -> r
listFold forEmpty forCons [] = forEmpty
listFold forEmpty forCons (x  :                                   xs) =
                           x `forCons` (listFold forEmpty forCons xs)
--  forCons x (listFold forEmpty forCons xs)

-- >>> listFold 0 (+) list4
-- 22

-- >>> listFold 0 (\ x y -> x + y) list4
-- 22

data Optional a =
    Result a
  | Null
  deriving Show

-- Eq a: Constraint, "die Werte von a sind vergleichbar"
listIndex :: Eq a => a -> [a] -> Optional Integer
listIndex x [] = Null
listIndex x (y:ys) =
  if x == y
  then Result 0 
  else 
    fmap (+1) (listIndex x ys)
    -- optionalMap (\index -> index + 1) (listIndex x ys)
    {-
    case listIndex x ys of
      Null -> Null
      Result index -> Result (index + 1)
-}

listMap ::     (a -> b) ->         [a] ->        [b]
listMap f [] = []
listMap f (x:xs) = f x : listMap f xs

optionalMap :: (a -> b) -> Optional a -> Optional b
optionalMap f Null = Null
optionalMap f (Result a) = Result (f a)

-- >>> :info Functor
-- type Functor :: (* -> *) -> Constraint
-- class Functor f where
--   fmap :: (a -> b) -> f a -> f b

instance Functor Optional where
  fmap :: (a -> b) -> Optional a -> Optional b
  fmap = optionalMap

-- Zutaten:
-- Typkonstruktor/Typ mit Typparameter f
-- fmap :: (a -> b) -> f a -> f b
-- fmap id = id
-- fmap f . fmap g = fmap (f . g) 

-- Eq ist eine Typklasse
-- Typklasse == Interface
-- Instanz == Implementierung

-- >>> :info Eq
-- type Eq :: * -> Constraint
-- class Eq a where
--   (==) :: a -> a -> Bool
-- instance Eq Integer -- Defined in ‘GHC.Num.Integer’

-- >>> listIndex 5 [1, 3, 5, 7]
-- Result 2

--- >>> listIndex Snake [Dog, Cat, Cat, Dog, Snake, Dog, Cat]
-- Result 4

instance Eq Pet where
  (==) :: Pet -> Pet -> Bool
  (==) Dog Dog = True
  (==) Cat Cat = True
  (==) Snake Snake = True
  (==) _ _ = False

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
--   {-# MINIMAL (+), (*), abs, signum, fromInteger, (negate | (-)) #-}
--   	-- Defined in ‘GHC.Num’
-- instance Num Double -- Defined in ‘GHC.Float’
-- instance Num Float -- Defined in ‘GHC.Float’
-- instance Num Int -- Defined in ‘GHC.Num’
-- instance Num Integer -- Defined in ‘GHC.Num’
-- instance Num Word -- Defined in ‘GHC.Num’

-- Typklassen: idealerweise für universelle Konzepte, d.h. aus der Mathematik

-- hier: 5 Konzepte aus der Algebra

-- - Halbgruppe
-- - Monoid

-- - Funktor
-- - applikativer Funktor
-- - Monad

-- "neutrales Element"

-- Algebraische Struktur:
-- - Menge(n)/Typ
-- - Operationen (auf dem Typ)
-- - Gleichungen

-- Assoziativität 
-- (a + b) + c = a + (b + c)
-- der Operation + auf der Menge R
-- + :: R -> R -> R

-- Integer
-- (*) :: Integer -> Integer -> Integer
-- (a * b) * c = a * (b * c)

-- Halbgruppe
-- Typ t
-- op :: t -> t -> t
-- op (op a b) c == op a (op b c)

-- Typ [a]
-- (++) :: [a] -> [a] -> [a]
-- (a ++ b) ++ c == a ++ (b ++ c)

-- Typ string
-- (: string-append (string string -> string)
-- (string-append (string-append a b) c) = (string-append a (string-append b c))

-- Shapes:
-- Shape
-- MkOverlap :: Shape -> Shape -> Shape
-- isIn (MkOverlap s1 (MkOverlap s2 s3)) == isIn (MkOverlap (MkOverlap s1 s2) s3)

-- image
-- (: overlay (image image -> image)
-- (overlay i1 (overlay i2 i3)) == (overlay (overlay i1 i2) i3)

-- nicht: Duschprodukte

class Semigroup t where
  -- op (op a b) c == op a (op b c)
  op :: t -> t -> t

instance Semigroup [a] where
  op :: [a] -> [a] -> [a]
  op = (++)

-- neutrales Element:
-- Voraussetzung Halbgruppe
-- Semigroup t
-- neutral :: t
-- op neutral x == op x neutral == x
-- Monoid: Halbgruppe mit neutralem Element

-- Pfeil falschrum!
class Semigroup t => Monoid t where
  neutral :: t

instance Monoid [a] where
  neutral :: [a]
  neutral = []

instance (Semigroup a, Semigroup b) => Semigroup (a, b) where
  op :: (a, b) -> (a, b) -> (a, b)
  op (a1, b1) (a2, b2) = (op a1 a2, op b1 b2)

instance (Monoid a, Monoid b) => Monoid (a, b) where
  neutral =  --    :: (a, b)
     (neutral,  -- :: a 
      neutral)  -- :: b

-- Übung:
-- instance ... Monoid (Optional a) where ...

instance (Semigroup a) => Semigroup (Optional a) where
  op :: Semigroup a => Optional a -> Optional a -> Optional a
  op Null x = x
  op x Null = x
  op (Result a) (Result a') = Result (op a a')

instance (Semigroup a) => Monoid (Optional a) where
  neutral :: Semigroup a => Optional a
  neutral = Null

newtype Additive a = MkAdditive a 

instance Num a => Num (Additive a) where
  (+) (MkAdditive a) (MkAdditive a') = MkAdditive (a + a')

instance Num a => Semigroup (Additive a) where
  op = (+)

-- data Maybe a = Nothing | Just a

opList :: Monoid r => [r] -> r
opList list = listFold neutral op list

-- >>> op "Mike" "Sperber" -- Strings sind Listen
-- "MikeSperber"

-- >>> opList ["Mike", "Sperber", "Hendrik"]
-- "MikeSperberHendrik"

-- >>> opList ([] :: [String])
-- ""

{-
// Java:
interface Semigroup<T> {
   T op(T other);
}

interface Monoid<T> extends Semigroup<T> {
   T neutral();
}

class MyList implements Semigroup<MyList> { ... }

// eher:
// "Halbgruppe von T"
interface Semigroup<T> {
   T op(T t1, T t2);
   T neutral();
}

class MyListSemigroup implements Semigroup<MyList> {
  ...
}

... Semigroup<MyList> myListSemigroup = new MyListSemigroup();
myListSemigroup.op(..., ...)
  

-}

incOptional :: Optional Integer -> Optional Integer
incOptional = fmap (+1)

plusOptional :: Optional Integer -> Optional Integer -> Optional Integer
plusOptional o1 o2 = fmap2 (+) o1 o2

fmap2 :: Applicative f => (a -> (x -> y)) -> f a -> f x -> f y -- b = x -> y
-- fmap2 fn fa fx = pure fn <*> fa <*> fx
-- fmap2 fn fa fx = fmap fn fa <*> fx
-- (<$>) = fmap
fmap2 fn fa fx = fn <$> fa <*> fx

fmap3 :: Applicative f => (a1 -> a2 -> a3 -> b) -> f a1 -> f a2 -> f a3 -> f b
fmap3 fn fa fx fy = pure fn <*> fa <*> fx <*> fy

-- >>> :info Applicative
-- type Applicative :: (* -> *) -> Constraint
-- class Functor f => Applicative f where
--   pure :: a -> f a
--   (<*>) :: f (a -> b) -> f a -> f b

instance Applicative Optional where
  pure = Result
  (<*>) :: Optional (a -> b) -> Optional a -> Optional b
  (<*>) Null Null = Null
  (<*>) Null (Result a) = Null
  (<*>) (Result f) Null = Null
  (<*>) (Result f) (Result a) = Result (f a)

-- fmap  ::   (a -> b) -> f a -> f b
-- (<*>) :: f (a -> b) -> f a -> f b

-- Validierung:
-- "Make illegal states unrepresentable." -> nur valide Domänenbjekte bauen
-- "invalide" bzw. "ungecheckte" Objekte bekommen einen anderen Typ

data Validation a = 
    Valid a
  | Invalid [String]
  deriving Show

-- Aufgabe:
-- 1. Applicative-Instanz für Validation
-- 2. Kleines Beispiel: Person mit Alter und Name, >18, Name < 100 Buchstaben
instance Functor Validation where
  fmap :: (a -> b) -> Validation a -> Validation b
  fmap f (Valid a) = Valid (f a)
  fmap f (Invalid errors) = Invalid errors

instance Applicative Validation where
  pure :: a -> Validation a
  pure = Valid
  (<*>) :: Validation (a -> b) -> Validation a -> Validation b
  (<*>) (Valid fa) (Valid a) = Valid (fa a)
  (<*>) (Valid _) (Invalid errors) = Invalid errors
  (<*>) (Invalid errors) (Valid _) = Invalid errors
  (<*>) (Invalid errors1) (Invalid errors2) = Invalid (errors1 ++ errors2)


-- class Stream<T>:
-- <R> Stream<R> flatMap(Function<T, Stream<R>> mapper)
-- ... bind

data AdultAge = AdultAge Integer
  deriving Show

data Person = MkPerson String AdultAge
  deriving Show


validateAge :: Integer -> Validation AdultAge
validateAge age = if age >= 18 then Valid (AdultAge age) else Invalid ["too young"]

validateName :: String -> Validation String
validateName name = if length name > 10 then Invalid ["name too long"] else Valid name

validatePerson :: String -> Integer -> Validation Person
validatePerson name age = MkPerson <$> (validateName name) <*> (validateAge age)

-- >>> validatePerson "Mikeeeeeeeeeee" 15
-- Invalid ["name too long","too young"]


-- fmap  ::        (a ->   b) -> f a -> f b
-- (<*>)      :: f (a ->   b) -> f a -> f b
-- flip (>>=) ::   (a -> f b) -> f a -> f b
-- (>>=) :: f a -> (a -> f b) -> f b

-- Arrows: auch zwischen Funktoren und Monaden
