module Intro where

-- Typsignatur
x :: Integer
x = 5

y :: Integer
y = x + 12

-- >>> f 17
f a = a + x + y

f' :: Integer -> Integer
f' = \ a -> a + x + y

-- Ein Haustier ist eins der folgenden:
-- - Hund
-- - Katze
-- - Schlange
-- Typ: Pet
-- Konstruktoren: Dog / Cat / Snake
data Pet = Dog | Cat | Snake
  -- deriving: mach Standard-Instanz für Standard-Typklasse
  deriving (Eq, Show)

{-
instance Eq Pet where
    Dog == Dog = True
    Cat == Cat = True
    Snake == Snake = True
    _ == _ = False
-}

-- Ist ein Haustier niedlich?
-- >>> isCute Dog
-- True

-- data Bool = True | False

isCute :: Pet -> Bool
isCute Dog = True
isCute Cat = True
isCute Snake = False

{-
interface Pet { ... }
class Dog implements Pet { ... }
class Cat implements Pet { ... }
class Snake implements Pet { ... }

Java: 4 Typen

3 Konstruktoren: new Dog, new Cat, new Snake

Pet dog = new Dog()

Haskell:

1 Typ: Pet
-}

-- Ein Gürteltier hat folgende Eigenschaften:
-- - lebendig oder tot
-- - Gewicht

data Liveness = Dead | Alive
  deriving Show

-- Typsynonym
type Weight = Integer

{-
-- Record
data Dillo = MkDillo { dilloLiveness :: Liveness, 
                       dilloWeight :: Weight }
  deriving Show

dillo1 :: Dillo
dillo1 = MkDillo {dilloLiveness = Alive, dilloWeight = 10}
dillo2 :: Dillo
dillo2 = MkDillo Dead 12

-- Faustregel:
-- - Variablen klein
-- - "Konstante" groß

-- Gürteltier überfahren
runOverDillo :: Dillo -> Dillo
-- >>> runOverDillo dillo1
-- MkDillo {dilloLiveness = Dead, dilloWeight = 10}
-- >>> runOverDillo dillo2
-- MkDillo {dilloLiveness = Dead, dilloWeight = 12}
-- runOverDillo dillo = 
--    MkDillo { dilloLiveness = Dead, dilloWeight = dilloWeight dillo }
-- "functional update"
-- runOverDillo dillo = dillo { dilloLiveness = Dead }
-- pattern matching
-- runOverDillo (MkDillo { dilloLiveness = l, dilloWeight = w}) =
--    MkDillo { dilloLiveness = Dead, dilloWeight = w}
runOverDillo (MkDillo _ w) = MkDillo Dead w
-}

-- Ein Tier auf dem texanischen Highway ist eins der folgenden:
-- - Gürteltier
-- - Papagei
-- gemischte Daten: in Haskell alle Fälle in ein "data"

data Animal =
    MkDillo { dilloLiveness :: Liveness, 
              dilloWeight :: Weight }
  | Parrot String Weight
  deriving Show

dillo1 :: Animal
dillo1 = MkDillo { dilloLiveness = Alive, dilloWeight = 10}
dillo2 :: Animal
dillo2 = MkDillo Dead 12

parrot1 :: Animal
parrot1 = Parrot "Hallo!" 1
parrot2 :: Animal
parrot2 = Parrot "Goodbye!" 2

-- Tier überfahren
runOverAnimal :: Animal -> Animal
-- >>> runOverAnimal dillo1
-- MkDillo {dilloLiveness = Dead, dilloWeight = 10}
-- >>> runOverAnimal parrot1
-- Parrot "" 1
runOverAnimal (MkDillo _liveness weight) = MkDillo Dead weight
runOverAnimal (Parrot _ weight) = Parrot "" weight

-- Tier füttern
-- >>> feedAnimal dillo1 5
-- MkDillo {dilloLiveness = Alive, dilloWeight = 15}
-- >>> feedAnimal parrot1 7
-- Parrot "Hallo!" 8
feedAnimal :: Animal -> (Weight -> Animal)
feedAnimal dillo@(MkDillo liveness weight) amount = -- Alias-Pattern
    case liveness of -- pattern matching
        Dead -> dillo -- MkDillo liveness weight
        Alive -> MkDillo liveness (weight + amount)
feedAnimal (Parrot sentence weight) amount =
    Parrot sentence (weight + amount)

{-
feedAnimal' :: Weight -> (Animal -> Animal)
feedAnimal' amount dillo@(MkDillo liveness weight) =
        case liveness of -- pattern matching
        Dead -> dillo -- MkDillo liveness weight
        Alive -> MkDillo liveness (weight + amount)
feedAnimal' amount (Parrot sentence weight) =
    Parrot sentence (weight + amount)
-}

-- swap :: (Animal -> (Weight -> Animal)) -> (Weight -> (Animal -> Animal))
-- Typvariablen
-- Higher-Order-Funktion, eingebaut als flip
swap :: (a -> b -> c) -> (b -> a -> c)
-- swap f = \ b -> \ a -> f a b
-- swap f = \ b a -> f a b
swap f b a = f a b
-- swap f b a = f a b

feedAnimal' :: Weight -> Animal -> Animal
-- >>> feedAnimal' 5 dillo1
-- MkDillo {dilloLiveness = Alive, dilloWeight = 15}
feedAnimal' = swap feedAnimal

feedAnimal'' :: (Animal, Weight) -> Animal
-- >>> feedAnimal''(dillo1, 5)
-- MkDillo {dilloLiveness = Alive, dilloWeight = 15}

-- (dillo1, 5) ist ein Tupel, Ad-hoc-zusammengesetzte Daten
feedAnimal''(dillo@(MkDillo liveness weight), amount) =
  -- Alias-Pattern
  case liveness of -- pattern matching
    Dead -> dillo -- MkDillo liveness weight
    Alive -> MkDillo liveness (weight + amount)
feedAnimal''(Parrot sentence weight, amount) =
  Parrot sentence (weight + amount)

-- >>> tuplify (swap feedAnimal) (5, dillo1)
-- MkDillo {dilloLiveness = Alive, dilloWeight = 15}
-- >>> tuplify feedAnimal (dillo1, 5)
-- MkDillo {dilloLiveness = Alive, dilloWeight = 15}

-- Idee: Haskell Curry
--       Moses Schönfinkel
-- eingebaut als uncurry
tuplify :: (a -> (b -> c)) -> ((a, b) -> c)
tuplify f = \(a, b) -> f a b

-- eingebaut als curry
untuplify :: ((a, b) -> c) -> (a -> (b -> c))
untuplify f a b = f (a, b)

-- Eine geometrische Figur ("Shape") ist eins der folgenden:
-- - ein Kreis
-- - ein Quadrat
-- - eine Überlagerung zweiter geometrischer Figuren

-- Entwickle Datenrepräsentation für geometrische Figuren! 
-- Schreibe eine Funktion, die feststellt, ob ein Punkt in
-- einer geometrischen Figur liegt oder außerhalb.

-- Eine Liste ist eins der folgenden:
-- - die leere Liste
-- - eine Cons-Liste aus erstem Element und Rest-Liste
{-
data ListOf a =
    Empty 
  | Cons a (ListOf a)
  deriving Show

list1 :: ListOf Integer
list1 = Cons 5 Empty
list2 :: ListOf Integer
list2 = Cons 8 (Cons 5 Empty)
list3 :: ListOf Integer
list3 = Cons 7 list2

listSum :: ListOf Integer -> Integer
-- >>> listSum list3
-- 20
listSum Empty = 0
listSum (Cons first rest) = first + (listSum rest)
-}

-- eingebaute Listen:
-- leere Liste: []
-- cons: : (Infix)

list1 :: [Integer]
list1 = 5 : []
list2 :: [Integer]
list2 = 8 : (5 : [])
list3 :: [Integer]
list3 = 7 : list2
list4 :: [Integer]
list4 = [4, 7, 8, 5]

listSum :: [Integer] -> Integer
-- >>> listSum list4
-- 24
listSum [] = 0
listSum (first:rest) = first + (listSum rest)

listMap :: (a -> b) -> [a] -> [b]
-- >>> listMap (\ x -> x * 2) [1,2,3,4]
-- [2,4,6,8]
listMap f [] = []
listMap f (first:rest) = (f first) : (listMap f rest)

listCopy :: [a] -> [a]
listCopy [] = []
listCopy (first:rest) = first : (listCopy rest)

listFold :: b -> (a -> b -> b) -> [a] -> b
listFold e f [] = e
listFold e f (first : rest) = first `f` (listFold e f rest) -- `f` : Infix

-- strikte Auswertung / call-by-value 
-- Bei einem Funktionsaufruf werden die Argumente ausgewertet,
-- bevor der Rumpf der Funktion ausgewertet wird.
-- Java, C#, F#, Racket, Python

-- nicht-strikt: das Gegenteil
-- Ausdrücke werden erst ausgewertet, wenn der Wert benötigt wird.

-- ghc (Haskell-Compiler): lazy evaluation
-- nicht-strikt, berechnet nix doppelt


-- alle natürlichen Zahlen, angefangen bei n
natsFrom :: Integer -> [Integer]
natsFrom n = n : natsFrom (n+1)

-- Vielfache einer Zahl aus einer Liste von Zahlen streichen
strikeMultiples :: Integer -> [Integer] -> [Integer]
-- >>> strikeMultiples 2 [2,3,4,5,6,7,8]
-- [3,5,7]
strikeMultiples n list =
    filter (\ n' -> n' `mod` n /= 0) list

sieve :: [Integer] -> [Integer]
sieve [] = []
sieve (p:rest) = p : (sieve (strikeMultiples p rest))

primes :: [Integer]
primes = sieve (natsFrom 2)

data Optional a =
    Null
  | Result a
  deriving Show

-- Index eines Elements in einer Liste berechnen
-- >>> listIndex 5 [3, 4, 7, 5, 11]
-- Result 3
listIndex :: Eq a => a -> [a] -> Optional Integer 
listIndex x [] = Null
listIndex x (first:rest) =
    if x == first
    then Result 0
    else 
      case listIndex x rest of 
        Null -> Null
        Result index -> Result (index + 1)

-- Eq a: Constraint, Einschränkung von möglichen Typen, die für a eingesetzt werden
-- Eq a => : "Wenn a die Eigenschaft Eq hat"

-- Speziell Constraint Eq: Typklasse (denke "Interface")
-- Implementierung einer Typklasse: "instance"
{-
class Eq a where
  (==) :: a -> a -> Bool
  (/=) :: a -> a -> Bool
-}

