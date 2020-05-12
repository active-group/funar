{-# OPTIONS_GHC -fwarn-incomplete-patterns #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE KindSignatures #-}
module Intro where

import Prelude hiding (Monoid, Semigroup, Functor)

x :: Integer
x = 5

-- Aggregatzustand
data State = -- data: neuer Datentyp
  Solid | Liquid | Gas
  deriving (Show, Eq) -- generiert Typklassen-Instanzen für Show, Eq

s1 :: State
s1 = Solid
s2 :: State
s2 = Liquid
s3 :: State
s3 = Gas

-- Aggregatzustand von Wasser ausrechnen
waterState :: Integer -> State
waterState temp =
  if temp < 0
  then Solid
  else if temp <= 100
  then Liquid
  else Gas

waterState' temp
  | temp < 0    = Solid
  | temp <= 100 = Liquid
  | otherwise   = Gas

-- algebraischer Datentyp: 3 Klassen / 3 Konstruktoren
-- Typen, Konstruktoren: Großbuchstaben
data Pet = Hund | Katze | Schlange
  deriving (Show, Eq)

-- data Bool = True | False

-- Ist Haustier niedlich?
isCute :: Pet -> Bool

isCute Hund = True
isCute Katze = True
isCute Schlange = False

-- Tiere auf dem texanischen Highway

-- Gürteltiere
-- Konstruktor: Pflicht

data Dillo = Dillo1 { alive :: Bool, weight :: Integer }
        --   ^^^^^ Konstruktor, heißt genauso wie der Typ
 deriving Show

-- Satz, Gewicht
{-
data Parrot = Parrot String Integer
  deriving Show
-}
-- funktioniert nicht: data Animal = Dillo | Parrot

data Liveness = Dead | Alive
  deriving (Show, Eq)

data Animal weight =
    Dillo { dilloAlive :: Liveness, dilloWeight :: weight }
  | Parrot String weight
  deriving (Show, Eq)

dillo1 = Dillo { dilloAlive = Alive, dilloWeight = (kg 10) }
dillo2 = Dillo Dead (kg 12)

data Weight = Kg Integer
  deriving Show

instance Num Weight where
  (Kg w1) + (Kg w2) = Kg (w1 + w2)

kg :: Integer -> Weight
kg quantity
  | quantity >= 0 = Kg quantity
  | otherwise = undefined

-- Tier überfahren
-- runOverAnimal dillo@(Dillo False weight) = dillo
-- runOverAnimal (Dillo _ weight) = Dillo False weight
-- runOverAnimal (Dillo { dilloAlive = alive, dilloweight = weight}) = undefined
-- runOverAnimal dillo@(Dillo {}) = Dillo False (dilloWeight dillo)
runOverAnimal :: Animal weight -> Animal weight
runOverAnimal dillo@(Dillo {}) = dillo { dilloAlive = Dead }
runOverAnimal (Parrot sentence weight) = Parrot "" weight

-- Tier füttern
-- feedAnimal :: Integer -> (Animal -> Animal)
-- geschönfinkelte/currifizierte Funktion
feedAnimal :: Num weight => weight -> Animal weight -> Animal weight
feedAnimal amount (Dillo liveness weight) = Dillo liveness (weight + amount)
feedAnimal amount (Parrot sentence weight) = Parrot sentence (weight + amount)

-- feedAnimal = \ amount -> \ (Parrot sentence weight) = Parrot sentence (weight + amount)

-- feedAnimal' :: (Integer, Animal) -> Animal
feedAnimal' :: Num weight => (weight, Animal weight) -> Animal weight
feedAnimal' (amount, (Dillo liveness weight)) = Dillo liveness (weight + amount)
feedAnimal' (amount, (Parrot sentence weight)) = Parrot sentence (weight + amount)

-- feedAnimal' -> feedAnimal
-- curryA :: ((Integer, Animal) -> Animal) -> (Integer -> Animal -> Animal)
{-
curryA f =
  \ weight ->
    \ animal ->
      f (weight, animal)
-}
-- Kleinbuchstabe: (Typ-)Variable
curryA :: ((a, b) -> c) -> (a -> b -> c)
-- curryA f weight animal = f (weight, animal)
curryA f a b = f (a, b)

-- inverse curryA
uncurryA :: (a -> b -> c) -> ((a, b) -> c)
uncurryA f =
  \ (a, b) -> f a b

flipA :: (a -> b -> c) -> (b -> a -> c)
flipA f a b = f b a

flipT :: ((a, b) -> c) -> ((b, a) -> c)
-- flipT f (b, a) = f (a, b)
-- flipT f = uncurryA (flipA (curryA f))
flipT = uncurryA . flipA . curryA
-- Funktionskomposition o
-- (.) :: (b -> c) -> (a -> b) -> (a -> c)

-- (>>) :: (a -> b) -> (b -> c) -> (a -> c)

data IntList =
    EmptyIntList
  | ConsIntList Integer IntList

list0 = []
list1 = 1:[] -- : gesprochen "cons", 1elementige Liste mit 1
list1' = [1] -- gleiche Liste
list2 = 1:2:[]
list2' = [1, 2]

-- Summe aller Listenelemente
listSum :: [Integer] -> Integer
listSum [] = 0
listSum (first:rest) = first + (listSum rest)

-- Funktion auf alle Elemente einer Liste anwenden
listMap :: (a -> b) -> [a] -> [b]
listMap f [] = []
listMap f (x:xs) = (f x) : (listMap f xs)

-- natürliche Zahlen ab ...
-- lazy evaluation, nicht-strikt
-- strikt: bei einem Funktionsaufruf werden die Argumente ausgewertet,
-- bevor die Funktion aufgerufen wird.
-- nicht-strikt: ein Wert wird erst ermittelt, wenn er gebraucht
natsFrom :: Integer -> [Integer]
natsFrom n = n : natsFrom (n + 1)

-- Primzahlen ermitteln
strikeMultiples :: Integer -> [Integer] -> [Integer]
strikeMultiples n [] = []
strikeMultiples n (first:rest) =
  if rem first n == 0
  then strikeMultiples n rest
  else first : (strikeMultiples n rest)

strikeMultiples' n list = filter (\ x -> rem x n /= 0) list

-- Vorbedingung: erstes Element ist Primzahl
sieve :: [Integer] -> [Integer]
sieve [] = []
sieve (first:rest) = first : (sieve (strikeMultiples first rest))

primes :: [Integer]
primes = sieve (natsFrom 2)

y = if 1 > 2 then 5 else 7 -- 1 > 2 ? 5 : 7

z = let x = 5
        y = 7
    in x + y

data Map key value = Map [(key, value)]
  -- deriving Eq

instance (Eq key, Eq value) => Eq (Map key value) where
  map1 == map2 = (mapSubset map1 map2) && (mapSubset map2 map1)

mapSubset :: (Eq key, Eq value) => Map key value -> Map key value -> Bool
mapSubset (Map []) (Map []) = True
mapSubset (Map (x1:xs1)) (Map []) = False
mapSubset (Map []) (Map (x2:xs2)) = True
mapSubset (Map ((key1, value1):xs1))  map2 =
  case mapGet map2 key1 of
    NotThere -> False
    There value2 -> 
      let xs1' = filter (\ (key1', value1') -> key1 /= key1') xs1
      in (value1 == value2) && (mapSubset (Map xs1') map2)


map1 = Map [(1, "Mike"), (2, "Marcello")]
map2 = Map [(2, "Marcello"), (1, "Mike")]

map3 = Map [(2, "Marcello"), (2, "Nathan"), (1, "Mike")]

{-
class Eq a where
  (==) :: a -> a -> Bool
-}

-- optionalMap :: (a -> b) -> Optional a -> Optional b
-- listMap     :: (a -> b) -> [a]        -> [b]
-- listMap     :: (a -> b) -> List a     -> List b
-- suggeriert Abstraktion über Optional / List / Map key
-- universalMap :: (a -> b) -> f a -> f b
mapMap :: (a -> b) -> (Map key) a -> (Map key) b
mapMap f (Map []) = Map []
mapMap f (Map ((key1, value1):rest)) =
  Map ((key1, f value1) : unMap (mapMap f (Map rest)))

unMap :: Map key value -> [(key, value)]
unMap (Map list) = list

class Functor (f :: * -> *) where
  -- universalMap identity x = x
  -- universalMap f (universalMap g x) = universalMap (f . g) x 
  universalMap :: (a -> b) -> f a -> f b -- a :: *, b :: *, f a :: *

identity x = x

instance Functor Optional where
  universalMap = optionalMap

instance Functor [] where
  universalMap = listMap

instance Functor (Map key) where
  universalMap = mapMap

-- instance Functor Animal where
--   universalMap = undefined

data Optional a =
    NotThere
  | There a
  deriving (Eq, Show)

optionalMap :: (a -> b) -> Optional a -> Optional b
optionalMap f NotThere = NotThere
optionalMap f (There a) = There (f a)

{-
data Maybe a = Nothing | Just a
-}

-- Eintrag in der Map nachschauen
-- Eq key : Einschränkung, Constraint
-- "vergleichbar"
mapGet :: forall key value . Eq key => Map key value -> key -> Optional value
mapGet (Map []) key = NotThere
mapGet (Map ((key', value'):rest)) key =
  if key == key'
  then There value'
  else mapGet (Map rest) key

-- neutrales Element bezüglich der Addition über den natürlichen Zahlen
-- Menge M, Operation o : M x M -> M

-- Assoziativgesetz (a + b) + c = a + (b + c)
--                  (a * b) * c = a * (b * c)
--                  (a && b) && c = a && (b && c)

-- Halbgruppe

class Semigroup t where
  -- combine (combine a b) c == combine a (combine b c)
  combine :: t -> t -> t

-- ungünstig:
instance Semigroup Integer where
  combine = (+)

data Additive = Additive Integer

instance Semigroup Additive where
  combine (Additive a) (Additive b) = Additive (a + b)

data Multiplicative = Multiplicative Integer

instance Semigroup Multiplicative where
  combine (Multiplicative a) (Multiplicative b) = Multiplicative (a * b)

-- Monoid: Semigroup + neutrales Element
class Semigroup t => Monoid t where
  -- combine neutral x == combine x neutral == x
  neutral :: t

-- Gruppe ... Ring ...
