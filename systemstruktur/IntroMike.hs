{-# OPTIONS_GHC -fwarn-incomplete-patterns #-}
{-# LANGUAGE InstanceSigs #-}
module Intro where

x = 5

-- Zahl verdoppeln
double :: Integer -> Integer
double x = x * 2

-- Aggregatzustand von Wasser ist eins der folgenden:
-- fest, flüssig, gas
data State = -- neuer Datentyp:  data
  Fest | Flüssig | Gas -- muß groß!
  deriving Show

-- Aggregatzustand von Wasser berechnen
waterState' :: Double -> State
waterState' temp =
  if temp <= 0
  then Fest
  else if temp <= 100
  then Flüssig
  else Gas

{-
waterState temp
  | temp <= 0   = Fest
  | temp <= 100 = Flüssig
  | otherwise   = Gas
-}

waterState temp | temp <= 0 = Fest
waterState temp | temp <= 100 = Flüssig
waterState temp = Gas

-- typische Temperatur für Aggregatzustand von Wasser berechnen
typicalTemperature :: State -> Double
typicalTemperature Fest = -20 
typicalTemperature Flüssig = 20
typicalTemperature Gas = 101

-- Gürteltier hat folgende Eigenschaften:
-- - lebendig oder tot
-- - Gewicht
data Liveness = Dead | Alive
    deriving (Show, Eq)

{-
data Dillo = Dillo { dilloLiveness :: Liveness,
                     dilloWeight :: Integer } 
   deriving Show

d1 = Dillo { dilloLiveness = Alive, dilloWeight = 20 } -- Gürteltier, lebendig, 20kg
d2 = Dillo Dead 30 -- Gürteltier tot, 30kg

-- Gürteltier überfahren
runOverDillo :: Dillo -> Dillo
-- runOverDillo d = Dillo { dilloLiveness = Dead, dilloWeight = dilloWeight d }
runOverDillo d@(Dillo Dead _) = d -- Alias-Pattern 
runOverDillo (Dillo _ weight) = Dillo Dead weight

-- Gürteltier füttern
feedDillo :: Dillo -> (Integer -> Dillo)
feedDillo d@(Dillo Dead weight) amount = d
feedDillo (Dillo Alive weight) amount = Dillo Alive (weight + amount)

-- feedDillo' :: Integer -> Dillo -> Dillo
-- feedDillo' amount d@(Dillo Dead weight) = d
-- feedDillo' amount (Dillo Alive weight) = Dillo Alive (weight + amount)
feedDillo' = flip feedDillo

myFlip :: (a -> b -> c) -> (b -> a -> c)
myFlip f b a = f a b

o f g = \ x -> f (g x)

feedDillo'' (d@(Dillo Dead weight), amount) = d
feedDillo'' ((Dillo Alive weight), amount) = Dillo Alive (weight + amount)
-}

-- eingebaut: curry
fromTupled :: ((a, b) -> c) -> (a -> b -> c)
-- fromTupled f = \ a -> \ b -> f (a, b) 
fromTupled f a b = f (a, b)

-- eingebaut: uncurry
toTupled :: (a -> b -> c) -> ((a, b) -> c)
-- toTupled f = \ (a, b) -> f a b
toTupled f (a, b) = f a b

-- "point-free": keine Variablen
flipTupled = uncurry . flip . curry 

-- highway = [d1, d2]

-- data Parrot = Parrot String Integer
--     deriving Show

-- parrotSentence (Parrot sentence _) = sentence

{-
data Animal =
    Dillo' Dillo
  | Parrot' Parrot
-}
-- algebraische Datentypen
-- gemischte Daten von zusammengesetzten Daten
data Animal =
    Dillo { dilloLiveness :: Liveness,
            dilloWeight :: Integer }
  | Parrot String Integer
  deriving Show

instance Eq Animal where
  (Dillo liveness1 weight1) == (Dillo liveness2 weight2) =
    (liveness1 == liveness2) && (weight1 == weight2)
  (Parrot sentence1 weight1) == (Parrot sentence2 weight2) =
    (sentence1 == sentence2) && (weight1 == weight2)
  _ == _ = False

d1 = Dillo { dilloLiveness = Alive, dilloWeight = 20 } -- Gürteltier, lebendig, 20kg
d2 = Dillo Dead 30 -- Gürteltier tot, 30kg

p1 = Parrot "Mike ist doof!" 10
p2 = Parrot "Der Schatz ist auf Madagaskar!" 20

runOverAnimal :: Animal -> Animal
runOverAnimal (Dillo _ weight) = Dillo Dead weight
runOverAnimal (Parrot _ weight) = Parrot "" weight

listSum :: [Integer] -> Integer
listSum [] = 0
listSum (x:xs) = -- erstes Element x, Rest xs
  x + listSum xs


listFold :: b -> (a -> b -> b) -> [a] -> b
listFold forEmpty forCons [] = forEmpty
listFold forEmpty forCons (x:xs) =
  forCons x (listFold forEmpty forCons xs)

-- Typklasse: Eigenschaft von Typ(en)
class FSemigroup a where
  -- combine muß assoziativ sein
  combine :: a -> a -> a

-- combine :: FSemigroup a => a -> a -> a
--            ^^^^^^^^^^^^ Constraint / Forderung

-- Beweis der Eigenschaft 
instance FSemigroup [a] where
  combine l1 l2 = l1 ++ l2

instance (FSemigroup x, FSemigroup y) => FSemigroup (x, y) where
  combine :: (x, y) -> (x, y) -> (x, y)
  combine (x1, y1) (x2, y2) = (combine x1 x2, combine y1 y2)

class FSemigroup a => FMonoid a where
  neutral :: a

instance FMonoid [a] where
  neutral = []

-- data Maybe a = Nothing | Just a

listMap :: (a -> b) -> [a] -> [b]
listMap f [] = []
listMap f (x:xs) = (f x) : (listMap f xs)

maybeMap :: (a -> b) -> Maybe a -> Maybe b
maybeMap f Nothing = Nothing
maybeMap f (Just a) = Just (f a)

class FFunctor f where -- f = [...], f = Maybe
  -- ffmap f (ffmap g x) = ffmap (f . g) x
  -- ffmap identity x = x
  ffmap :: (a -> b) -> f a -> f b

instance FFunctor Maybe where
  ffmap = maybeMap

instance FFunctor [] where
  ffmap = listMap

-- strikt: Argumente werden ausgewertet, bevor die Funktion aufgerufen wird
-- call-by-value
-- Scala call-by-name f(x : => X)
-- nicht-strikt, ghc: lazy evaluation

-- alle natürlichen Zahlen ab n auflisten
natsFrom :: Integer -> [Integer]
natsFrom n = n : (natsFrom (n + 1))

strikeMultiples :: Integer -> [Integer] -> [Integer]
strikeMultiples n lis =
  filter (\ n' -> n' `rem` n /= 0) lis

-- erste Zahl Primzahl
sieve :: [Integer] -> [Integer]
sieve [] = []
sieve (x:xs) = x : (sieve (strikeMultiples x xs))

primes = sieve (natsFrom 2)