module Intro where

import Prelude hiding (Semigroup, Monoid, Functor)
{-
Haskell: späte 80er Jahre
getypt, lazy evaluation

Haskell-Standard 2010

inzwischen gibt es nur ghc "Glasgow Haskell Compiler"

Haskell 2010 + viele Erweiterungen

-}

-- Kleinschreibung: Variable
-- Großschreibung : "Konstante"

x :: Integer
x = 10
y :: Integer
y = x + 11
z :: Integer
z = x * y

-- Haustier ist eins der folgenden:
-- - Hund
-- - Katze
-- - Schlange

-- data: eigener, neuer Datentyp
-- Pet: Typ
-- Dog, Cat, Snake: Konstuktoren / Fälle / Klassen
data Pet = Dog | Cat | Snake
  deriving Show -- damit die Werte in der REPL angezeigt werden

{-
class Eq a where
    (==) :: a -> a -> Bool
-}

instance Eq Pet where
    (==) Dog Dog = True 
    (==) Cat Cat = True 
    (==) Snake Snake = True 
    (==) _ _ = False

-- Ist ein Haustier niedlich?
isCute :: Pet -> Bool 
isCute Dog   = True
isCute Cat   = True 
isCute Snake = False

-- Gürteltier hat folgende Eigenschaften:
-- - tot oder lebendig
-- - Gewicht
data Liveness = Dead | Alive 
  deriving (Show, Eq)

-- Typsynonym
type Weight = Double

{-
-- 2 Dinge, die Dillo heißen: Typ, Konstruktor
data Dillo = Dillo { dilloLiveness :: Liveness, dilloWeight :: Weight }
  deriving Show

dillo1 :: Dillo
dillo1 = Dillo { dilloLiveness = Alive, dilloWeight = 10 } -- lebendiges Gürteltier, 10kg
dillo2 :: Dillo
dillo2 = Dillo Dead 12 -- totes Gürteltier, 12kg

-- Gürteltier überfahren
runOverDillo :: Dillo -> Dillo
-- runOverDillo dillo = Dillo { dilloLiveness = Dead, dilloWeight = dilloWeight dillo }
-- runOverDillo Dillo { dilloLiveness = l, dilloWeight = w} =
 --  Dillo { dilloLiveness = Dead, dilloWeight = w}
-- runOverDillo (Dillo {dilloLiveness = _, dilloWeight = w}) =
--  Dillo {dilloLiveness = Dead, dilloWeight = w}
-- runOverDillo (Dillo {dilloWeight = w}) =
--  Dillo {dilloLiveness = Dead, dilloWeight = w}
-- runOverDillo (Dillo _ w) = Dillo Dead w
-- runOverDillo dillo = dillo { dilloLiveness = Dead } -- Kopie von dillo, mit geänderten Feldern
runOverDillo = \ (Dillo _ w) -> Dillo Dead w


-- Gürteltier füttern
feedDillo :: Weight -> (Dillo -> Dillo)
feedDillo = \ amount -> \ dillo -> dillo { dilloWeight = dilloWeight dillo + amount }

data Parrot = Parrot String Weight

parrot1 = Parrot "Der Schatz ist im Silbersee!" 2 -- Piratenpapagei, 2kg

-}

-- algebraischer Datentyp
data Animal =
    Dillo { dilloLiveness :: Liveness, dilloWeight :: Weight }
  | Parrot String Weight
--  | SpecificParrot Parrot
  deriving Show

data Parrot = Ara | NorwegianBlue -- ...
  deriving Show

dillo1 :: Animal
dillo1 = Dillo {dilloLiveness = Alive, dilloWeight = 10} -- lebendiges Gürteltier, 10kg

dillo2 :: Animal
dillo2 = Dillo Dead 12 -- totes Gürteltier, 12kg

parrot1 :: Animal
parrot1 = Parrot "Der Schatz ist im Silbersee!" 2 -- Piratenpapagei, 2kg

-- Tier überfahren
runOverAnimal :: Animal -> Animal
runOverAnimal (Dillo _ weight) = Dillo Dead weight
runOverAnimal (Parrot _ weight) = Parrot "" weight 

-- Tier füttern
feedAnimal :: Weight -> (Animal -> Animal)
-- feedAnimal amount (Dillo Alive weight) = Dillo Alive (weight + amount)
-- feedAnimal amount (Dillo Dead weight) = Dillo Dead weight
feedAnimal amount (Dillo liveness weight) =
    case liveness of
        Alive -> Dillo Alive (weight + amount)
        Dead -> Dillo Dead weight
feedAnimal amount (Parrot sentence weight) = Parrot sentence (weight + amount)

feedAnimal5 :: Animal -> Animal
feedAnimal5 = feedAnimal 5

-- Tupel, Ad-hoc-zusammengesetzte Daten
feedAnimal' :: (Weight, Animal) -> Animal
{-
feedAnimal' (amount, Dillo liveness weight) =
  case liveness of
    Alive -> Dillo Alive (weight + amount)
    Dead -> Dillo Dead weight
feedAnimal' (amount, Parrot sentence weight) = Parrot sentence (weight + amount)
-}
feedAnimal' (amount, animal) = feedAnimal amount animal

-- f' (amount, animal) = f amount animal
-- Haskell Curry
-- Moses Schönfinkel
-- uncurry
entschönfinkeln :: (a -> b -> c) -> ((a, b) -> c)
entschönfinkeln f (a, b) = f a b

-- curry
schönfinkeln :: ((a, b) -> c) -> (a -> b -> c)
schönfinkeln f a b = f (a, b)

-- o / Funktionskomposition
o :: (b -> c) -> (a -> b) -> (a -> c)
-- o f g = \ a -> f (g a)
o f g a = f (g a)

-- eingebaut: .
-- (f . g) a = f (g a)

-- Eine Liste ist eins der folgenden:
-- - leere Liste
-- - Cons-Liste bestehend aus erstem Element und Rest-Liste
{-
data ListOf element =
    Empty
  | Cons element (ListOf element)
-}

-- list1' :: ListOf Integer
-- list1' = Cons 4 (Cons 7 (Cons 3 Empty))

-- leere Liste: []
-- Cons: :

listSum :: [Integer] -> Integer 
listSum [] = 0 
listSum (first:rest) = 
    first + (listSum rest)

type ListOf a = [a]

listMap :: (a -> b) -> ListOf a -> ListOf b
listMap f [] = []
listMap f (x:xs) =
    (f x) : (listMap f xs)  


listFold :: b -> (a -> b -> b) -> [a] -> b 
listFold forEmpty forCons [] = forEmpty
listFold forEmpty forCons (a:as) = 
   forCons a (listFold forEmpty forCons as)

-- Dictionary / Key-/Value-Store
data Map key value = Map [(key, value)]

map1 :: Map String String
map1 = Map [("Mike", "Sperber"), ("Micha", "Riedlinger")]

map2 :: Map Pet String
map2 = Map [(Dog, "Dog"), (Cat, "Cat"), (Snake, "Snake")]

data Optional a =
    Absent | Present a
    deriving Show

instance Semigroup a => Semigroup (Optional a) where
    combine Absent Absent = Absent 
    combine (Present a) Absent = Present a 
    combine Absent (Present b) = Present b 
    combine (Present a1) (Present a2) = Present (a1 `combine` a2)

instance Semigroup a => Monoid (Optional a) where
    neutral = Absent

optionalMap :: (a -> b) -> Optional a -> Optional b
optionalMap f Absent = Absent
optionalMap f (Present a) = Present (f a)

class Functor f where
  universalMap :: (a -> b) -> f a -> f b

instance Functor Optional where
    universalMap = optionalMap

instance Functor [] where
    universalMap = listMap

-- Eq key: Constraint
-- Eq : Typklasse
-- bedeutet: Werte des Typs keys sind vergleichbar mit ==
mapLookup :: Eq key => key -> Map key value -> Optional value
mapLookup key (Map []) = Absent 
mapLookup key (Map ((key', value'):rest)) = 
    if key == key'
    then Present value'
    else mapLookup key (Map rest)
    
-- (: + (number number -> number))
-- (: * (number number -> number))
-- (: and (boolean boolean -> boolean))
-- (: overlay (image image -> image))
-- binärer Kombinator

-- Assoziativität
-- a + (b + c) = (a + b) + c
-- a * (b * c) = (a * c) * c
-- a and (b and c) = (a and b) and c
-- (overlay a (overlay b c)) = (overlay (overlay a b) c)

-- Zutaten: Menge/Typ, binärer Kombinator, Assoziativgesetz
-- Halbgruppe / Semigroup
-- Menge/Typ + Operationen + Gesetze/Gleichungen

class Semigroup a where
    -- a `combine` (b `combine` c) == (a `combine` b) `combine` c
    combine :: a -> a -> a

-- Monoid: Halbgruppe + neutrales Element
class Semigroup a => Monoid a where
    -- neutral `combine` a == a `combine` neutral == a
    neutral :: a

instance Semigroup [e] where
    combine list1 list2 = list1 ++ list2

instance Monoid [e] where
    neutral = []

instance (Semigroup a, Semigroup b) => Semigroup (a, b) where
    combine (a1, b1) (a2, b2) = (a1 `combine` a2, b1 `combine` b2)

instance Semigroup (Map key value) where
    -- rechtslastig
    combine (Map list1) (Map list2) = Map (list2 ++ list1)
    