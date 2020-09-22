{-# OPTIONS_GHC -fwarn-incomplete-patterns #-}
module Intro where

import Prelude hiding (Monoid, Semigroup, Functor)

-- ghcide

x :: Integer
x = 7

f :: Integer -> Integer
f n = n + 1

data Pet = Hund | Katze | Schlange 
  deriving (Show, Eq)

pet1 :: Pet
pet1 = Hund

-- Ist Haustier niedlich?
isCute :: Pet -> Bool
isCute Hund = True
isCute Katze = True
isCute Schlange = False

-- Typsynonym
type Weight = Integer 

data Liveness = Dead | Alive
  deriving Show 

-- Ein Gürteltier hat folgende Eigenschaften:
-- - tot oder lebendig
-- - Gewicht
{-
data Dillo = Dillo { dilloLiveness :: Liveness, 
                     dilloWeight :: Weight }

-- GÜrteltier, lebendig, 12kg
dillo1 :: Dillo
dillo1 = Dillo { dilloLiveness = Alive, dilloWeight = 12 }
dillo2 :: Dillo
dillo2 = Dillo Dead 10 -- totes Gürteltier, 10kg

-- Gürteltier überfahren
runOverDillo :: Dillo -> Dillo
-- Schreibweise 1:
-- runOverDillo dillo = Dillo { dilloLiveness = Dead, dilloWeight = dilloWeight dillo }
-- Schreibweise 2:
runOverDillo (Dillo _ weight) = Dillo { dilloLiveness = Dead, dilloWeight = weight }

-- Papagei hat folgende Eigenschaften:
-- - Satz
-- - Gewicht
data Parrot = Parrot String Weight

parrot1 :: Parrot
parrot1 = Parrot "Hallo!" 10
parrot2 :: Parrot
parrot2 = Parrot "Der Schatz ist auf der Osterinsel!" 2
-}

-- Algebraischer Datentyp
-- Gemischte Daten aus zusammengesetzten Daten
data Animal =
    Dillo { dilloLiveness :: Liveness, 
            dilloWeight :: Weight }
  | Parrot String Weight
  deriving Show

dillo1 = Dillo { dilloLiveness = Alive, dilloWeight = 12 }
dillo2 = Dillo Dead 10
parrot1 :: Animal
parrot1 = Parrot "Hallo!" 10
parrot2 :: Animal
parrot2 = Parrot "Der Schatz ist auf der Osterinsel!" 2

parrotSentence :: Animal -> String
parrotSentence (Parrot sentence _) = sentence

-- Tier überfahren
runOverAnimal :: Animal -> Animal
runOverAnimal (Dillo { dilloWeight = weight }) = Dillo Dead weight
runOverAnimal (Parrot _ weight) = Parrot "" weight

{-
feedAnimal :: Weight -> (Animal -> Animal)
feedAnimal amount (Dillo liveness weight) = Dillo liveness (weight + amount)
feedAnimal amount (Parrot sentence weight) = Parrot sentence (weight + amount)
-}

feedAnimal' :: (Weight, Animal) -> Animal -- Tupel
feedAnimal' (amount, Dillo liveness weight) = Dillo liveness (weight + amount)
feedAnimal' (amount, Parrot sentence weight) = Parrot sentence (weight + amount)

-- Currifizieren
-- currify :: ((Weight, Animal) -> Animal) -> (Weight -> (Animal -> Animal))
currify :: ((a, b) -> c) -> (a -> (b -> c))
-- currify f = \ a -> (\ b -> f (a, b)) 
currify f a b = f (a, b)

feedAnimal :: Weight -> Animal -> Animal
feedAnimal = currify feedAnimal'

data ListOfIntegers =
      EmptyList
    | Cons Integer ListOfIntegers

data ListOf element =
    EmptyList'
  | Cons' element (ListOf element)

listSum :: [Integer] -> Integer
listSum [] = 0
listSum (first : rest) = first + (listSum rest)

type List a = [a]

listMap :: (a -> b) -> List a -> List b
listMap f [] = []
listMap f (x:xs) = (f x) : (listMap f xs)

y = let x = 5
        g y = y + 7
    in g (x + 1)

natsFrom :: Integer -> [Integer]
natsFrom n = n : (natsFrom (n + 1))

-- Vielfache einer Zahl aus einer Liste streichen
strikeMultiples :: Integer -> [Integer] -> [Integer]
strikeMultiples n xs = filter (\ n' -> n' `mod` n /= 0) xs

-- Mache aus Liste von Zahlen Liste von Primzahlen
-- Annahme: erste Zahl der Liste ist schon Primzahl
sieve :: [Integer] -> [Integer]
sieve (first : rest) = first : sieve (strikeMultiples first rest)

primes :: [Integer]
primes = sieve (natsFrom 2)

data Map key value = Map [(key, value)]

map1 :: Map Pet String
map1 = Map [(Hund, "niedlich")]

data Optional a =
    Result a
  | NoResult

optionalMap :: (a -> b) -> Optional a -> Optional b
optionalMap f (Result a) = Result (f a)
optionalMap f NoResult = NoResult


-- "Es gibt eine Eigenschaft"
class HasEquality t where
  equals :: t -> t -> Bool

{-
Äquivalenzrelation:

"Menge" t
equals :: t -> t -> Bool
 
Transitivität
a `equals` b && b `equals` c => a `equals` c

Symmetrie
a `equals` b == b `equals` a 

Reflexivität
a `equals` a == True

-}

instance HasEquality Pet where
  equals Katze Katze = True
  equals Hund Hund = True
  equals Schlange Schlange = True
  equals _ _ = False

instance (HasEquality a, HasEquality b) => HasEquality (a, b) where
  equals (a1, b1) (a2, b2) =
    (a1 `equals` a2) && (b1 `equals` b2)

-- Eq key : Constraint / Eigenschaft / Typklasse
mapGet :: Eq key => Map key value -> key -> Optional value
mapGet (Map []) key = NoResult
mapGet (Map ((key', value') : rest)) key =
  if key == key'
  then Result value'
  else mapGet (Map rest) key

{-
- Injektivität / Bijektivität

+ :: a -> a -> a

Assoziativität
(a + b) + c = a + (b + c)
^^^ Halbgruppe

neutrales Element:
a + n = n + a = a
^^^ Monoid

Kommutativität
a + b = b + a

Zutaten:
Menge + Operationen + Eigenschaften
-}

class Semigroup t where
  -- combine ist assoziativ
  combine :: t -> t -> t

class Semigroup t => Monoid t where
  -- neutral ist neutrales Element
  neutral :: t

data Additive = Additive Integer

instance Semigroup Additive where
  combine (Additive n1) (Additive n2) = Additive (n1 + n2)

maxStringLength = maximum `o` (map length)

-- Funktionskomposition
o :: (b -> c) -> (a -> b) -> a -> c
o f g = \ x -> f (g x)

class Functor f where
  -- universalMap id f == f
  universalMap :: (a -> b) -> f a -> f b

instance Functor Optional where
  universalMap = optionalMap

instance Functor [] where
  universalMap = listMap
