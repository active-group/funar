module Intro where

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

instance Eq Pet 

-- Ist ein Haustier niedlich?
isCute :: Pet -> Bool 
isCute Dog   = True
isCute Cat   = True 
isCute Snake = False

-- Gürteltier hat folgende Eigenschaften:
-- - tot oder lebendig
-- - Gewicht
data Liveness = Dead | Alive 
  deriving Show

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
data ListOf element =
    Empty
  | Cons element (ListOf element)

list1' :: ListOf Integer
list1' = Cons 4 (Cons 7 (Cons 3 Empty))

-- leere Liste: []
-- Cons: :

listSum :: [Integer] -> Integer 
listSum [] = 0 
listSum (first:rest) = 
    first + (listSum rest)

listMap :: (a -> b) -> [a] -> [b]
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

-- Eq key: Constraint
-- Eq : Typklasse
-- bedeutet: Werte des Typs keys sind vergleichbar mit ==
mapLookup :: Eq key => key -> Map key value -> Optional value
mapLookup key (Map []) = Absent 
mapLookup key (Map ((key', value'):rest)) = 
    if key == key'
    then Present value'
    else mapLookup key (Map rest)
    