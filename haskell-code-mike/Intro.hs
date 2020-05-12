{-# OPTIONS_GHC -fwarn-incomplete-patterns #-}
module Intro where

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

data Animal =
    Dillo { dilloAlive :: Liveness, dilloWeight :: Integer }
  | Parrot String Integer
  deriving (Show, Eq)

dillo1 = Dillo { dilloAlive = Alive, dilloWeight = 10 }
dillo2 = Dillo Dead 12

-- Tier überfahren
runOverAnimal :: Animal -> Animal
-- runOverAnimal dillo@(Dillo False weight) = dillo
-- runOverAnimal (Dillo _ weight) = Dillo False weight
-- runOverAnimal (Dillo { dilloAlive = alive, dilloweight = weight}) = undefined
-- runOverAnimal dillo@(Dillo {}) = Dillo False (dilloWeight dillo)
runOverAnimal dillo@(Dillo {}) = dillo { dilloAlive = Dead }
runOverAnimal (Parrot sentence weight) = Parrot "" weight

-- Tier füttern
feedAnimal :: Integer -> (Animal -> Animal)
-- geschönfinkelte/currifizierte Funktion
feedAnimal amount (Dillo liveness weight) = Dillo liveness (weight + amount)
feedAnimal amount (Parrot sentence weight) = Parrot sentence (weight + amount)

-- feedAnimal = \ amount -> \ (Parrot sentence weight) = Parrot sentence (weight + amount)

feedAnimal' :: (Integer, Animal) -> Animal
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

