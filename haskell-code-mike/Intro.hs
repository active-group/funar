module Intro where

-- Typsignatur
x :: Integer
x = 12

y :: Integer
y = x + 5

f :: Integer -> Integer
-- >>> f 13 
-- 20
f x = x + 7

f' :: Integer -> Integer
f' = \ x -> x + 7

g :: Integer -> Integer -> Integer
g x y = (x + y) * 2

g' :: Integer -> Integer -> Integer
g' = \ x -> \ y -> (x + y) * 2

h x y =
    let d = x - y
    in d * d

tuple3 :: (Pet, Animal, Bool)
tuple3 = (Dog, dillo1, True)
-- Ein Haustier ist eins der folgenden:
-- - Hund
-- - Katze
-- - Schlange
-- data: neuer Datentyp
-- Pet: Typ
-- Dog, Cat, Snake: Konstruktoren
data Pet = Dog | Cat | Snake
  deriving Show

-- instance: Implementierung einer Typklasse für einen Typ
-- (==): Vergleich in "normaler" Präfix-Notation
instance Eq Pet where -- "Pet hat die Eigenschaft Eq"
   (==) = eqPet

eqPet :: Pet -> Pet -> Bool
eqPet Dog Dog = True
eqPet Cat Cat = True
eqPet Snake Snake = True
eqPet _ _ = False

-- data Bool = True | False

-- Ist ein Haustier niedlich?
-- >>> isCute Dog
-- True

-- >>> isCute Snake
-- False

isCute :: Pet -> Bool
isCute Dog = True
isCute Cat = True
isCute Snake = False

-- Ein Gürteltier hat folgende Eigenschaften:
-- - tot - ODER - lebendig
-- - Gewicht

data Liveness = Dead | Alive
  deriving Show

-- Typsynonym
type Weight = Integer

{-
data Dillo =
    MkDillo { dilloLiveness :: Liveness,
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

-- Schablone:
-- runOverDillo dillo =
--    MkDillo { dilloLiveness = undefined, dilloWeight = undefined }
--    dilloLiveness dillo  ... dilloWeight dillo

-- runOverDillo dillo =
--   MkDillo {dilloLiveness = Dead, dilloWeight = dilloWeight dillo}
-- runOverDillo dillo =
--    MkDillo Dead (dilloWeight dillo)
-- runOverDillo (MkDillo { dilloLiveness = l, dilloWeight = w }) =
--    MkDillo Dead w
-- runOverDillo (MkDillo { dilloWeight = w }) =
--    MkDillo Dead w
-- runOverDillo (MkDillo _ w) = MkDillo Dead w
runOverDillo dillo = dillo { dilloLiveness = Dead }

-- Ein Tier ist eins der folgenden:
-- - Gürteltier
-- - Papagei
data Parrot = MkParrot String Weight
  deriving Show

parrot1 = MkParrot "Hello" 1
-}

-- Haskell: gemischte Daten müssen in *einer* data-Definition
-- algebraischer Datentyp
data Animal =
    MkDillo { dilloLiveness :: Liveness, dilloWeight :: Weight }
  | MkParrot String Weight
  deriving Show

{-
#define DILLO 1
#define PARROT 2

struct animal {
    int tag;
    union {
        struct { liveness ... weight_t }
        struct { char[] ... weight_t }
    }
}

-}

{-
interface Animal { 
    ... 
}
class Dillo implements Animal { ... }
class Parrot implements Animal { ... }

class AnimalFactory {
    Animal mkDillo(...) { return new Dillo(...); }
    Animal mkParrot(...) { return new Parrot(...); }
}

-}

dillo1 :: Animal
dillo1 = MkDillo { dilloLiveness = Alive, dilloWeight = 10 }
dillo2 :: Animal
dillo2 = MkDillo Dead 8

parrot1 :: Animal
parrot1 = MkParrot "Hello" 1
parrot2 :: Animal
parrot2 = MkParrot "Goodbye" 2

-- Tier überfahren
runOverAnimal :: Animal -> Animal
-- Schablone:
-- runOverAnimal (MkDillo liveness weight) = undefined
-- runOverAnimal (MkParrot sentence weight) = undefined

-- >>> runOverAnimal dillo1
-- MkDillo {dilloLiveness = Dead, dilloWeight = 10}

-- >>> runOverAnimal parrot1
-- MkParrot "" 1
runOverAnimal (MkDillo liveness weight) = MkDillo Dead weight
runOverAnimal (MkParrot sentence weight) = MkParrot "" weight

{-
animal_t run_over_animal(animal_t animal) {
    switch (animal.tag) {
        case DILLO: ...
        case PARROT: ...
    }
}

-}

-- >>> feedAnimal dillo1 5
-- MkDillo {dilloLiveness = Alive, dilloWeight = 15}

-- >>> feedAnimal parrot1 2
-- MkParrot "Hello" 3

-- alle Haskell-Funktionen sind einstellig
feedAnimal :: Animal -> (Weight -> Animal)
-- Alias-Pattern
feedAnimal dillo@(MkDillo Dead weight) amount = dillo
feedAnimal (MkDillo Alive weight) amount = MkDillo Alive (weight+amount)
{-
feedAnimal (MkDillo liveness weight) amount =
    case liveness of
        Dead -> MkDillo liveness weight
        Alive -> MkDillo liveness (weight + amount)
-}
feedAnimal (MkParrot sentence weight) amount =
    MkParrot sentence (weight + amount)

feedAnimal' :: Weight -> Animal -> Animal
-- Alias-Pattern
feedAnimal' amount dillo@(MkDillo Dead weight)= dillo
feedAnimal' amount (MkDillo Alive weight) = MkDillo Alive (weight + amount)
{-
feedAnimal (MkDillo liveness weight) amount =
    case liveness of
        Dead -> MkDillo liveness weight
        Alive -> MkDillo liveness (weight + amount)
-}
feedAnimal' amount (MkParrot sentence weight) =
  MkParrot sentence (weight + amount)

-- swap :: (Animal -> Weight -> Animal) -> (Weight -> Animal -> Animal)
-- Kleinbuchstaben im Typ: Typvariablen
-- eingebaut als flip
swap :: (a -> b -> c) -> (b -> a -> c)
-- swap f = \ b -> \ a -> f a b
swap f b a = f a b

-- eingebaut uncurry
tuplify :: (a -> b -> c) -> ((a, b) -> c)
-- tuplify f = -- f :: a -> b -> c
--  \ (a, b) -> f a b
tuplify f (a, b) = f a b

-- Moses Schönfinkel
-- Haskell B. Curry
-- eingebaut curry
untuplify :: ((a, b) -> c) -> (a -> (b -> c))
--untuplify f a b = f (a, b)
untuplify f = -- f :: (a, b) -> c
     \a -> \b -> f (a, b)

-- Funktionskomposition
o :: (b -> c) -> (a -> b) -> (a -> c)
o bc ab = \a -> bc (ab a)

-- eingebaut unter dem Namen .
-- in Haskell: Namen, die aus Sonderzeichen bestehen, werden infix geschrieben

-- Tupel
feedAnimal'' :: (Weight, Animal) -> Animal
-- >>> feedAnimal''(3, dillo1)
-- MkDillo {dilloLiveness = Alive, dilloWeight = 13}
feedAnimal''(amount, MkDillo liveness weight) =
    case liveness of
        Alive -> MkDillo Alive (weight + amount)
        Dead -> MkDillo liveness weight
feedAnimal''(amount, MkParrot sentence weight) =
    MkParrot sentence (weight+amount)

-- Ein Duschprodukt ist eins der folgenden:
-- - Seife (pH-Wert)
-- - Shampoo (Farbe, Haartyp)

-- Ein Duschgel besteht aus:
-- - prozentualer Anteil Seife
-- - prozentualer Anteil Shampoo

-- Eine geometrische Figur (Shape) ist eins der folgenden:
-- - ein Kreis
-- - Quadrat
-- - eine Überlappung zweier geometrischer Figuren

-- Ist ein Punkt innerhalb einer geometrischen Figur?

-- Eine Liste ist eins der folgenden:
-- - die leere Liste
-- - eine Cons-Liste aus erstem Element und Rest-Liste
data ListOf element =
    Empty
  | Cons element (ListOf element)
  deriving Show

{-
struct List {
    int tag;
    union {
        struct empty {}
        struct cons { element ... List }
    }
}
-}

{-

list1 = Cons 5 Empty
list2 = Cons 6 (Cons 3 Empty)
list3 = Cons 5 list2
list4 = Cons 8 list3

-- Elemente einer Liste summieren
listSum :: ListOf Integer -> Integer
-- >>> listSum list4
-- 22
listSum Empty = 0
listSum (Cons first rest) =
    first + (listSum rest)
-}

-- Eingebaute Listen:
-- - die leere Liste: []
-- - Cons-Liste first:rest

list1 = 5 : []
list2 = 6 : (3 : [])
list3 = [5, 6, 3] -- wie list
list4 = 8 : list3

listSum :: [Integer] -> Integer
-- >>> listSum list4
-- 22
listSum [] = 0
-- listSum (first : rest) = 
--    first + (listSum rest)
listSum nonEmptyList =
    let first = head nonEmptyList
        rest = tail nonEmptyList
    in first + listSum rest

listMap :: (a -> b) -> [a] -> [b]
listMap f [] = []
-- listMap f (first:rest) = 
--    (f first) : (listMap f rest)
listMap f nonEmptyList =
    let first = head nonEmptyList
        rest = tail nonEmptyList
    in (f first) : (listMap f rest)

first2 :: [a] -> (a, a)
first2 (first:(second:rest)) = (first, second)
first2 _ = error "list not long enough"

strikeMultiples :: Integer -> [Integer] -> [Integer]
-- >>> strikeMultiples 2 [1,2,3,4,5,6,7,8]
-- [1,3,5,7]
{-
strikeMultiples n [] = []
strikeMultiples n (first:rest) =
    if mod first n == 0
    then strikeMultiples n rest
    else first : strikeMultiples n rest
-}
strikeMultiples n list =
    filter (\e -> mod e n /= 0) list

-- lazy evaluation
-- strikt: Argumente auswerten vor Funktionsaufruf
-- lazy/nicht-strikt: Argumente erst auswerten, wenn sie benötigt werden
natsFrom :: Integer -> [Integer]
natsFrom n = n : (natsFrom (n+1))

-- Annahme: erstes Element der Eingabeliste ist Primzahl
sieve :: [Integer] -> [Integer]
sieve [] = []
sieve (first:rest) = first : (sieve (strikeMultiples first rest))

primes = sieve (natsFrom 2)

-- >>> listSum' [1,2,3,4,5] 0
-- 15
listSum' [] acc = acc
listSum' (first:rest) acc =
    listSum' rest (acc + first)

-- Index eines Elements in einer Liste
-- soll 2
-- >>> listIndex 3 [10,9,3,2,5]
-- Result 2
-- >>> listIndex 5 [1,2,3]
-- Null

data Optional a =
    Null
  | Result a
  deriving Show

-- Fehlerfall:
-- - Exception
-- - -1
-- - null

-- Eq a: Constraint / Einschränkung
-- "a muß ein vergleichbarer Typ sein"
-- "a muß die Funktion == unterstützen"

listIndex :: Eq a => a -> [a] -> Optional Integer
listIndex x [] = Null
listIndex x (first:rest) =
--   ... first ... (listIndex x rest) ...
  if x == first
  then Result 0
  else
    case listIndex x rest of
        Null -> Null
        Result index -> Result (index + 1)

{-
Eq ist eine sogenannte Typklasse. => (aus OO) denke "Interface"

class Eq a where
  (==) :: a -> a -> Bool
-}