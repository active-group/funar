{-# LANGUAGE InstanceSigs #-}
module Intro where

import Prelude hiding (Semigroup, Monoid)

x :: Integer
x = 7

y :: Integer
y = x * 12 + 13

f :: Integer -> Integer
-- >>> f 12
-- 24
f = \ n -> n * 2

-- Abkürzung:
f' :: Integer -> Integer
-- >>> f' 12
-- 24
f' n = n * 2

-- Ein Haustier ist eins der folgenden:
-- - Katze - ODER -
-- - Hund - ODER -
-- - Schlange
-- ==> neuer Typ
data Pet = Cat | Dog | Snake
 deriving (Eq, Show)

eqPet :: Pet -> Pet -> Bool
eqPet Cat Cat = True
eqPet Dog Dog = True
eqPet Snake Snake = True
eqPet _ _ = False

{-
instance Eq Pet where
    (==) = eqPet
-}

-- Ist Haustier niedlich?
isCute :: Pet -> Bool 
-- 1 Gleichung pro Fall
-- >>> isCute Dog
-- True
-- >>> isCute Snake
-- False
isCute Cat = True
isCute Dog = True
isCute Snake = False

-- Gürteltier hat folgende Eigenschaften:
-- - lebendig oder tot
-- - Gewicht

data Liveness = Alive | Dead
  deriving Show

-- Typsynonym
type Weight = Integer

{-
-- Record-Definition
data Dillo = MkDillo { dilloLiveness :: Liveness,
                       dilloWeight :: Weight }
  deriving Show

-- lebendiges Gürteltier, 10kg
dillo1 :: Dillo
dillo1 = MkDillo { dilloLiveness = Alive, dilloWeight = 10 }

-- totes Gürteltier, 8kg
dillo2 :: Dillo
dillo2 = MkDillo Dead 8

-- Gürteltier überfahren
runOverDillo :: Dillo -> Dillo
-- >>> runOverDillo dillo1
-- MkDillo {dilloLiveness = Dead, dilloWeight = 10}
-- >>> runOverDillo dillo2
-- MkDillo {dilloLiveness = Dead, dilloWeight = 8}
-- runOverDillo dillo =
    -- Schablone: dilloLiveness dillo  dilloWeight dillo
--    MkDillo { dilloLiveness = Dead, dilloWeight = dilloWeight dillo }
-- runOverDillo (MkDillo { dilloLiveness = l, dilloWeight = w}) =
--    MkDillo { dilloLiveness = Dead, dilloWeight = w }
-- runOverDillo (MkDillo _ w) = MkDillo Dead w
-- "functional update"
-- "Kopie von dillo, ein Feld anders"
runOverDillo dillo = dillo { dilloLiveness = Dead }

-}

-- Ein Tier ist eins der folgenden:
-- - Gürteltier
-- - Papagei

data Animal =
    MkDillo { dilloLiveness :: Liveness, dilloWeight :: Weight }
  | MkParrot String Weight
  deriving Show

dillo1 :: Animal
dillo1 = MkDillo { dilloLiveness = Alive, dilloWeight = 10 }
dillo2 :: Animal
dillo2 = MkDillo Dead 8

parrot1 :: Animal
parrot1 = MkParrot "Hello!" 1
parrot2 :: Animal
parrot2 = MkParrot "Goodbye!" 2

-- Tier überfahren
runOverAnimal :: Animal -> Animal
-- >>> runOverAnimal dillo1
-- MkDillo {dilloLiveness = Dead, dilloWeight = 10}
-- >>> runOverAnimal parrot1
-- MkParrot "" 1

-- _ : "don't care"
runOverAnimal (MkDillo _ w) = MkDillo Dead w
runOverAnimal (MkParrot _ weight) = MkParrot "" weight

-- nur 1stellige Funktionen in Haskell!

-- Tier füttern
feedAnimal :: Animal -> (Weight -> Animal)
-- >>> (feedAnimal dillo1) 2
-- MkDillo {dilloLiveness = Alive, dilloWeight = 12}
feedAnimal dillo@(MkDillo liveness weight) amount = -- Alias-Pattern
    case liveness of
        Dead -> dillo -- MkDillo liveness weight
        Alive -> MkDillo liveness (weight + amount)
feedAnimal (MkParrot sentence weight) amount =
    MkParrot sentence (weight + amount)

feedAnimal' :: Weight -> (Animal -> Animal)
feedAnimal' amount dillo@(MkDillo liveness weight) =
  -- Alias-Pattern
  case liveness of
    Dead -> dillo -- MkDillo liveness weight
    Alive -> MkDillo liveness (weight + amount)
feedAnimal' amount (MkParrot sentence weight) =
  MkParrot sentence (weight + amount)

-- swap :: (Animal -> (Weight -> Animal)) -> (Weight -> (Animal -> Animal))
-- >>> :type swap feedAnimal
-- swap feedAnimal :: Weight -> Animal -> Animal
-- a, b, c: Typvariablen
swap :: (a -> b -> c) -> (b -> a -> c)
-- swap f = \ b -> \ a ->  f a b
-- swap f b = \ a -> f a b
-- eingebaut als flip
swap f b a = f a b

feedAnimal'' :: (Weight, Animal) -> Animal
-- >>> feedAnimal'' (1, dillo1)
-- MkDillo {dilloLiveness = Alive, dilloWeight = 11}
feedAnimal''(amount, dillo@(MkDillo liveness weight)) =
  -- Alias-Pattern
  case liveness of
    Dead -> dillo -- MkDillo liveness weight
    Alive -> MkDillo liveness (weight + amount)
feedAnimal''(amount, MkParrot sentence weight) =
  MkParrot sentence (weight + amount)

g :: Integer -> Integer -> Integer
g a b = a + b

tuplify :: (a -> b -> c) -> ((a, b) -> c)
-- >>> (tuplify feedAnimal) (dillo1, 2)
-- MkDillo {dilloLiveness = Alive, dilloWeight = 12}
-- eingebaut als uncurry
tuplify f =
    \ (a, b) -> f a b

-- >>> untuplify feedAnimal'' 2 dillo1
-- MkDillo {dilloLiveness = Alive, dilloWeight = 12}
-- eingebaut als curry
untuplify :: ((a, b) -> c) -> (a -> b -> c)
untuplify f =
    \ a -> \ b -> f (a, b)

schönfinkeln = untuplify
entschönfinkeln = tuplify

-- Duschprodukte:
-- - Seife (hat pH-Wert)
-- - Shampoo (hat Haartyp)
-- - Duschgel (50% Seife, 50% Shampoo)

-- Datenanalyse + Datentyp
-- Funktion, die den Seifenanteil berechnet

-- Erweiterung:
-- - Mixtur aus zwei Duschprodukten, beliebige Anteile

-- Mittagspause

-- Eine Liste ist eins der folgenden:
-- - die leere Liste
--   []
-- - eine Cons-Liste aus erstem Element und Rest-Liste
--   first : rest

{-
-- Typkonstruktor
data ListOf element =
    Empty 
  | Cons element (ListOf element)
  deriving Show

list1 :: ListOf Integer
list1 = Cons 5 Empty

list2 :: ListOf Integer
list2 = Cons 2 (Cons 5 Empty)

-}

list1 :: [Integer]
list1 = 5 : []
list2 :: [Integer]
list2 = 2 : (5 : [])
list3 :: [Integer]
list3 = [6, 2, 6]
list4 :: [Integer]
list4 = 7 : list3

listSum :: [Integer] -> Integer
listSum [] = 0
-- listSum (first:rest) = first + (listSum rest)
-- listSum (head:tail) = head + (listSum tail)
listSum (x:xs) = x + listSum xs

listFold :: b -> (a -> b -> b) -> [a] -> b
-- >>> listFold 0 (+) [1,2,3]
-- 6
-- >>> listFold 1 (*) [1,2,3,4]
-- 24
-- >>> listFold [] (:) [1,2,3,4]
-- [1,2,3,4]
listFold n op [] = n
listFold n op (x  :                  xs) = -- op x (listFold n op xs)
               x `op` (listFold n op xs)

-- lazy evaluation
-- vs. strikte Auswertung: 
-- bei einem Funktionsaufruf werden erst die Argumente ausgewertet,
-- dann wird erst zur Funktion gesprungen

-- in Haskell: Argumente werden erst ausgewertet, wenn sie benötigt werden

natsFrom :: Integer -> [Integer]
natsFrom n = n : natsFrom (n+1)

strikeMultiples :: Integer -> [Integer] -> [Integer]
-- >>> strikeMultiples 2 [1,2,3,4,5,6,7,8]
-- [1,3,5,7]
strikeMultiples n list =
    filter (\ n' -> mod n' n /= 0) list

sieve :: [Integer] -> [Integer]
sieve [] = []
sieve (p:ps) = p : (sieve (strikeMultiples p ps))

data Optional a =
    Null 
  | Result a
  deriving Show

safeDivide :: (Eq a, Fractional a) => a -> a -> Optional a
safeDivide n m =
  if m == 0
  then Null
  else Result (n/m)

-- instance Functor Optional where
--    fmap :: (a -> b) -> Optional a -> Optional b

instance Applicative Optional where
  -- pure :: a -> Optional a
  -- (<*>) :: Optional (a -> b) -> Optional a -> Optional b
  pure = Result
  Result f <*> Result a = Result (f a)
  _ <*> _ = Null

instance Monad Optional where
  return = Result
  Null >>= next = Null
  (Result a) >>= next = next a 

optionalMap2 :: (a -> (b -> c)) -> Optional a -> Optional b -> Optional c
-- optionalMap2 f (Result a) (Result b) = Result (f a b)
-- optionalMap2 f _ _ = Null
optionalMap2 f oa ob =
  -- (pure f) <*> oa <*> ob
  f <$> oa <*> ob

optionalMap3 f oa ob oc = f <$> oa <*> ob <*> oc

da :: Double -> Double -> Double -> Double -> Optional Double
da a b c d =
  do r1 <- safeDivide a b
     r2 <- safeDivide c d
     return (r1 + r2)

da' :: Double -> Double -> Double -> Double -> Optional Double
da' a b c d = optionalMap2 (+) (safeDivide a b) (safeDivide c d)

-- Index eines Elements in einer Liste ermitteln
-- Eq a: Constraint / Einschränkung
-- "a unterstützt Gleichheit / =="
listIndex :: Eq a => a -> [a] -> Optional Integer
-- >>> listIndex 2 [4,3,5,2,1]
-- Result 3
-- >>> listIndex Dog [Snake, Snake, Snake, Cat, Dog, Snake]
-- Result 4
listIndex e [] = Null
listIndex e (x:xs) =
  if x == e 
  then Result 0
  else 
--    optionalMap (\ index -> index + 1) (listIndex e xs)
    optionalMap (1+) (listIndex e xs)
{-
    case listIndex e xs of
        Null -> Null
        Result index -> Result (index+1)
-}

optionalMap :: (a -> b) -> Optional a -> Optional b
optionalMap f Null = Null
optionalMap f (Result a) = Result (f a)

type List a = [a]
listMap     :: (a -> b) -> List    a -> List      b    
-- listMap ::     (a -> b) -> [a]       -> [b]
listMap f [] = []
listMap f (x : xs) = f x : (listMap f xs)

instance Functor Optional where
  fmap = optionalMap

-- eingebaut als id
identity x = x 
-- fmap identiy = identity

-- eingebaut als "."
-- Funktionskomposition
o :: (b -> c) -> (a -> b) -> (a -> c)
o f g = \ a -> f (g a)

-- fmap (f . g) = (fmap f) . (fmap g)

-- Semigroup -> Monoid

-- brauchen Typparameter:
-- Functor -> Applicative -> Monad

{-
Typklasse: "Eigenschaft eines Typs" / definiert durch Methoden

class Eq a where -- denk "Interface"
  (==) :: a -> a -> Bool

Implementierung eines Interfaces: "instance"
-}

{-
Sinnvoll: Typklassen für "universelle" Abstraktionen

bisher:

- Show: "ausdrucken"
- Eq: Gleichheit
- Ord: (totale) Ordnung
- Num: numerische Operationen
-}

{-
Algebra:
- Typ T
- Operationen (mit Typsignaturen)
- Gesetze / Gleichungen

- Typ T
- Operationen: op :: T -> T -> T ("binärer Kombinator")
  Assoziativgesetz:
  op a (op b c) = op (op a b) c
  z.B. a + (b + c) == (a + b) + c
  ^^^ Halbgruppe

- neutrales Element:
-              neutral :: T
- op neutral x == op x neutral == x
  ^^^ Halbgruppe + neutrales Element = Monoid
-}

class Semigroup t where
    -- op a (op b c) = op (op a b) c
    op :: t -> t -> t

-- >>> op [1,2,3] [4,5,6]
-- [1,2,3,4,5,6]

instance Semigroup [a] where
    -- Assoziativgesetz check
    op list1 list2 = list1 ++ list2

-- (Doppelpfeil falschrum)
class Semigroup t => Monoid t where
  neutral :: t

instance Monoid [a] where
    neutral = []

-- >>> op ([1,2,3], [4,5,6]) ([7,8,9], [10,11])
-- ([1,2,3,7,8,9],[4,5,6,10,11])

instance (Semigroup a, Semigroup b) => Semigroup (a, b) where
    op (a1, b1) (a2, b2) = (op a1 a2, op b1 b2)

instance (Monoid a, Monoid b) => Monoid (a, b) where
    neutral = (neutral, neutral)

instance Semigroup a => Semigroup (Optional a) where
    op :: Semigroup a => Optional a -> Optional a -> Optional a
    op Null x = x
    op x Null = x
    op (Result a) (Result a') = Result (op a a')

instance Semigroup a => Monoid (Optional a) where
    neutral = Null -- Result neutral wäre falsch

-- data Maybe a = Nothing | Just a 