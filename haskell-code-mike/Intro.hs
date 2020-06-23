{-# OPTIONS_GHC -fwarn-incomplete-patterns #-}
module Intro where

import Prelude hiding (Monoid, Semigroup, Functor)

-- Typ
x :: Integer
x = 5

-- data: neuer Typ
-- Literale: groß geschrieben, Variablen: klein
-- | : "oder"

data Pet = Hund | Katze | Schlange
  deriving Show

pet1 :: Pet
pet1 = Hund

-- Ist Haustier niedlich?
isCute :: Pet -> Bool
isCute Hund = True
isCute Katze = True
isCute Schlange = False

isCute' :: Pet -> Bool
isCute' pet =
  case pet of 
    Hund -> True
    Katze -> True
    Schlange -> False

-- Zustand eines Gürteltiers zu einem bestimmten Zeitpunkt
data Liveness = Dead | Alive
  deriving (Show, Eq)

{-
data Dillo = Dillo { alive :: Liveness, weight :: Integer}
 deriving Show

-- Gürteltier, lebendig, 10kg
dillo1 :: Dillo
dillo1 = Dillo { alive = Alive, weight = 10}
-- Gürteltier, tot, 12kg
dillo2 :: Dillo
dillo2 = Dillo Dead 12

-- Gürteltier überfahren
runOverDillo :: Dillo -> Dillo
{-
runOverDillo dillo =
--  Dillo { alive = Dead, weight = weight dillo}
   Dillo Dead (weight dillo)
-}
-- runOverDillo (Dillo _ weight) = Dillo Dead weight
runOverDillo (Dillo { weight = w}) =
 Dillo { alive = Dead, weight = w }

data Parrot = Parrot String Integer
  deriving Show

parrot1 = Parrot "Hallo!" 2
-}

-- Algebraischer Datentyp:
-- gemischte Daten aus zusammengesetzte Daten
data Animal weight = 
    Dillo { alive :: Liveness, weight :: weight}
  | Parrot String weight
  deriving (Show, Eq)

dillo1 :: Animal Integer
dillo1 = Dillo { alive = Alive, weight = 10}
dillo2 :: Animal Integer
dillo2 = Dillo Dead 12
parrot1 :: Animal Integer
parrot1 = Parrot "Hallo!" 2
parrot2 :: Animal Integer
parrot2 = Parrot "Goodbye!" 1

data Weight = Kg Integer

instance Num Weight where
  (Kg a) + (Kg b) = Kg (a + b)

instance Functor Animal where
  universalMap f (Dillo alive weight) = Dillo alive (f weight)
  universalMap f (Parrot sentence weight) = Parrot sentence (f weight)

dillo1' = Dillo { alive = Alive, weight = Kg 10}

-- Tier überfahren
-- runOverAnimal :: Animal -> Animal
runOverAnimal :: Animal weight -> Animal weight
-- "runOverAnimal weiß nichts über weight =>
--  runOverAnimal beeinflußt das Gewicht nicht"
runOverAnimal (Dillo _ weight) = Dillo Dead weight
runOverAnimal (Parrot _ weight) = Parrot "" weight

-- Es gibt nur 1stellige Funktionen

-- Tier füttern
-- feedAnimal :: Integer -> (Animal -> Animal)
{-
feedAnimal amount (Dillo alive weight) = Dillo alive (weight + amount)
feedAnimal amount (Parrot sentence weight) = Parrot sentence (weight + amount)
-}
-- \ : "Lambda"
feedAnimal = \ amount -> \ animal ->
  case animal of
    Dillo alive weight -> Dillo alive (weight + amount)
    Parrot sentence weight -> Parrot sentence (weight + amount)

highway = [dillo1, parrot1, dillo2, parrot2]
deadHighway = map runOverAnimal highway
fedHighway = map (feedAnimal 1) highway


-- Tupel

-- feedAnimal' :: (Integer, Animal) -> Animal
feedAnimal' :: Num weight => (weight, Animal weight) -> Animal weight
feedAnimal' (amount, Dillo alive weight) = Dillo alive (weight + amount)
feedAnimal' (amount, Parrot sentence weight) = Parrot sentence (weight + amount)

data ListOfIntegers =
    Empty
  | Cons Integer ListOfIntegers

listSum :: [Integer] -> Integer
listSum [] = 0
listSum (first:rest) = first + (listSum rest)

-- (: list-map ((%a -> %b) (list-of %a) -> (list-of %b)))
listMap :: (a -> b) -> [a] ->   [b]
listMap    f           []     = []
listMap    f           (x:xs) = (f x) : (listMap f xs)

rev :: [a] -> [a]
rev xs0 = revHelper xs0 []
  where
    -- reversed: alle Elemente aus xs0 vor xs, in umgekehrter Reihenfolge
    revHelper :: [a] -> [a] -> [a]
    revHelper [] reversed = reversed
    revHelper (x:xs) reversed = revHelper xs (x:reversed)

fold :: b -> ((a, b) -> b) -> [a] -> b
fold empty _ [] = empty
fold empty reducer (x:xs) = (reducer (x, (fold empty reducer xs)))

foldLeft :: b -> (b -> a -> b) -> [a] -> b 
foldLeft init reducer xs = helper xs init
  where helper [] result = result 
        helper (x:xs) result = helper xs (reducer result x)

listMap' :: (a -> b) -> [a] -> [b]
listMap' f xs =
  fold [] (\ (listElement, acc) -> (f listElement) : acc) xs

-- Typsynonym
type List a = [a]

listMap'' :: (a -> b) -> List a -> List b
listMap'' f xs =
  rev (foldLeft [] (\ acc listElement -> (f listElement) : acc) xs)

exchange :: (a -> b -> c) -> (b -> a -> c)
-- f :: (a -> b -> c)
exchange    f =             \ xb  xa -> f xa xb
-- exchange = \ f -> \ b -> \ a -> f a b
-- exchange f b a = f a b

schönfinkeln :: ((a, b) -> c) -> (a -> b -> c)
schönfinkeln    f =
  \ a b -> f (a, b)

entschönfinkeln :: (a -> b -> c) -> ((a, b) -> c)
entschönfinkeln f (a, b) = f a b

exchange' :: ((a, b) -> c) -> ((b, a) -> c)
-- exchange' f = \ (b, a) -> f (a, b)

-- Funktionskomposition
o :: (b -> c) -> (a -> b) -> (a -> c)
o g f = \ a -> g (f a)

-- exchange' f = entschönfinkeln (exchange (schönfinkeln f))
-- exchange' f = (entschönfinkeln `o` exchange `o` schönfinkeln) f
exchange' = entschönfinkeln . exchange . schönfinkeln

-- nicht-strikte Auswertung / lazy evaluation

natsFrom :: Integer -> [Integer]
natsFrom n = n : (natsFrom (n + 1))

strikeMultiples :: Integer -> [Integer] -> [Integer]
strikeMultiples n xs =
  filter (\ x -> x `mod` n /= 0) xs

-- erste Zahl ist eine Primzahl
sieve :: [Integer] -> [Integer]
sieve [] = []
sieve (x:xs) =
  x : (sieve (strikeMultiples x xs))

data Map key value = Map [(key, value)]

mapMap :: (a -> b) -> (Map key) a -> (Map key) b
mapMap f (Map xs) = Map (map (\ (key, value) -> (key, f value)) xs)

-- Typklasse
-- class (Eq key, Eq value) => Eq (Map key value) where ...


map1 = [(Hund, "niedlich")]
map2 = [(Hund, "niedlich"), (Hund, "niedlich")]

data Optional a =
    There a
  | NotThere

optionalMap :: (a -> b) -> Optional a -> Optional b
optionalMap f (There a) = There (f a)
optionalMap f NotThere = NotThere

{-
class Eq a where
  (==) :: a -> a -> Bool

Äquivalenzrelation

Transitivität
(a == b) && (b == c) => a == c

Symmetrie
a == b => b == a

Reflexität
a == a

Kommutativität
a + b = b + a

instance Eq a => Eq [a] where
  [] == [] = True
  (x:xs) == (y:ys) =
    (x == y) && (xs == ys)
  _ == _ = False

instance (Eq a, Eq b) => Eq (a, b) where
  (a, b) == (a', b') = (a == a') && (b == b')
-}

instance Eq Pet where
  Katze == Katze = True
  Hund == Hund = True
  Schlange == Schlange = True
  _ == _ = False

mapGet :: Eq key => Map key value -> key -> Optional value
mapGet (Map []) key = NotThere
mapGet (Map ((key', value'):rest)) key =
  if key == key'
  then There value'
  else mapGet (Map rest) key

{-
data Maybe a =
    Nothing
  | Just a
-}

{-
Algebra: Menge + Operationen + Gleichungen
Programmieren: Typ + Operationen + Gleichungen

Beispiel:
Assoziativität
(a + b) + c = a + (b + c)
(a * b) * c = a * (b * c)
-}

-- Halbgruppe
class Semigroup a where
  -- Kombinator
  -- combine a (combine b c) == combine (combine a b) c
  combine :: a -> a -> a

-- Monoid
class Semigroup a => Monoid a where
  -- combine x neutral == combine neutral x == x
  neutral :: a


{-
class    - Typklasse ~~~~ Interface
instance - Instanz   ~~~~ Implementierung 
data     - Datentyp  ~~~~ Interface + Klassen
-}

data Additive = Additive Integer
data Multiplicative = Multiplicative Integer

instance Semigroup Additive where
  combine (Additive a) (Additive b) = Additive (a + b)

instance Monoid Additive where
  neutral = Additive 0


-- Monoid für Listen
-- Monoid für 2-Tupel

identity x = x

-- f Typkonstruktor
-- f :: * -> *
class Functor f where
  -- universalMap identity x == x
  -- universalMap identity = identity
  -- (universalMap f) . (universalMap g) == universalMap (f . g)
  universalMap :: (a -> b) -> f a -> f b

instance Functor [] where
  universalMap = listMap

instance Functor Optional where
  universalMap = optionalMap

instance Functor (Map key) where
  universalMap = mapMap

