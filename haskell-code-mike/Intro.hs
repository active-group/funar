module Intro where

import Prelude hiding (Semigroup, Monoid, Functor)

x :: Integer
x = 10

-- eigener Datentyp
data Pet = Dog | Cat | Snake
  deriving Show

-- "Eq Pet" bedeutet: Der Typ Pet hat die Eigenschaft Eq
-- => Es gibt eine Implementierung der Typklasse Eq für den Typ Pet-
-- => Die Funktion == ist definiert auf dem Typ Pet.

-- Implementierung einer Typklasse
instance Eq Pet where -- "Pet hat die Eigenschaft Eq, Pets sind vergleichbar"
  (==) Dog Dog = True
  (==) Cat Cat = True
  (==) Snake Snake = True
  (==) _ _ = False

-- Faustregel: Großbuchstaben - Konstante, Kleinbuchstaben - Variable

-- Ist Haustier niedlich?
isCute :: Pet -> Bool
isCute Dog = True
isCute Cat = True
isCute Snake = False

-- Typen und Werte haben unterschiedliche Namensräume

data Liveness = Dead | Alive
  deriving Show

-- Typsynonym
type Weight = Double

{-
-- Ein Gürteltier hat folgende Eigenschaften:
-- - lebendig oder tot
-- - Gewicht
data Dillo = Dillo { dilloLiveness :: Liveness,
                     dilloWeight :: Weight }
  deriving Show

-- lebendiges Gürteltier, 10kg
dillo1 :: Dillo
dillo1 = Dillo { dilloLiveness = Alive, dilloWeight = 10}

-- totes Gürteltier, 8kg
dillo2 :: Dillo
dillo2 = Dillo Dead 8

-- Gürteltier überfahren
runOverDillo :: Dillo -> Dillo
--- runOverDillo dillo = Dillo Dead (dilloWeight dillo)
-- runOverDillo (Dillo { dilloLiveness = liveness, dilloWeight = weight }) =
--     Dillo Dead weight
-- runOverDillo (Dillo _ weight) = Dillo Dead weight
runOverDillo d = d { dilloLiveness = Dead }
--                   ^^^^^^^ Kopie von dillo, aber dilloLiveness anders

-}

-- Alle Fälle eines Datentyps müssen in eine data-Definition

-- Ein Datentyp, zwei Klassen
-- algebraischer Datentyp
data Animal weight = 
    Dillo { dilloLiveness :: Liveness,
            dilloWeight :: weight }
  | Parrot String weight
  deriving Show

data MWeight = Kg Double
  deriving Show

instance Num MWeight where
  (Kg a) + (Kg b) = Kg (a + b)

dillo1' = Dillo Alive (Kg 10)

dillo1 :: Animal Weight
dillo1 = Dillo { dilloLiveness = Alive, dilloWeight = 10}

dillo2 :: Animal Weight
dillo2 = Dillo Dead 8

parrot1 :: Animal Weight
parrot1 = Parrot "Der Schatz ist Silbersee!" 1
parrot2 :: Animal Weight
parrot2 = Parrot "Tschüss!" 2

-- Tier überfahren
-- runOverAnimal (Dillo liveness weight) = Dillo Dead weight
runOverAnimal :: Animal weight -> Animal weight
runOverAnimal (Dillo { dilloWeight = weight}) = Dillo Dead weight
runOverAnimal (Parrot sentence weight) = Parrot "" weight

-- Tier füttern

-- Haskell kennt nur 1stellige Funktionen!
-- Hindley-Milner-Typsystem, 1stellige Funktionen, algebraische Datentypen: ML

feedAnimal :: Num weight => Animal weight -> weight -> Animal weight
feedAnimal (Dillo Alive weight) amount = Dillo Alive (weight + amount)
feedAnimal (Dillo Dead weight) amount = Dillo Dead weight 
feedAnimal (Parrot sentence weight) amount = Parrot sentence (weight + amount)

-- 1stellige Funktion, die ein 2-Tupel aus Animal und Weight als Argument akzeptiert
-- feedAnimal' :: (Animal, Weight) -> Animal
-- feedAnimal' (Dillo Alive weight, amount) = Dillo Alive (weight + amount)

-- tuplify :: (Animal -> Weight -> Animal) -> ((Animal, Weight) -> Animal)
tuplify :: (t1 -> t2 -> t3) -> ((t1, t2) -> t3)
--          ^^ Typvariable
tuplify f =
    \ (animal, weight) -> f animal weight

-- Moses Schönfinkel
-- Haskell Curry

-- schönfinkeln
currify :: ((a, b) -> c) -> (a -> b -> c)
-- currify f = \ a -> \ b -> f (a, b)
currify f a b = f (a, b)

-- entschönfinkeln
uncurrify ::  (a -> b -> c) -> ((a, b) -> c)
uncurrify f (a, b) = f a b

f :: Integer -> Integer -> Integer
f = \ a -> \ b -> a + b

feedAnimal' = tuplify feedAnimal

-- 2dimensionale Ebene

-- Eine geometrische Figur (Shape) ist eins der folgenden:
-- - ein Quadrat
-- - ein Kreis
-- - eine Überlappung zweier geometrischer Figuren

-- Eine Überlappung besteht aus:
-- - geometrische Figur
-- - noch 'ne geometrische Figur

data Shape = Square
           | Circle
           | Overlay Shape Shape

type Point = (Double, Double)

pointInShape :: Point -> Shape -> Bool
pointInShape p Square = undefined
pointInShape p Circle = undefined
pointInShape p (Overlay shape1 shape2) =
    (pointInShape p shape1) || (pointInShape p shape2)

-- 1. Aufgabe: Datendefinition -> Code
-- 2. Aufgabe: Funktion, die feststellt, 
--             ob ein Punkt innerhalb einer geometrischen Figur ist

g a b =
    let x = a + b
        y = a - b
    in x * y

-- Eine Liste ist eins der folgenden:
-- - leere Liste
-- - Cons-Liste aus erstem Element und Rest
data List element =
     Empty
   | Cons element (List element)
   deriving Show

-- list1 :: List Integer
-- list1 = Cons 1 (Cons 2 (Cons 3 Empty))

list1 :: [Integer]
list1 =    1 : (2 : (3 : []))
list1Sum = 1 + (2 + (3 + 0)) 
list1' = [1,2,3]

listSum' :: List Integer -> Integer
listSum' Empty = undefined
listSum' (Cons first rest) = first + (listSum' rest)

-- Elemente einer Liste addieren
listSum :: [Integer] -> Integer
listSum [] = 0
listSum (first:rest) = first + (listSum rest)

highway = [dillo1, parrot1, dillo2, parrot2]

type ListOf a = [a]

listMap :: (a -> b) -> ListOf a -> ListOf b
listMap f [] = []
listMap f (x:xs) =
    (f x) : (listMap f xs)

listFold :: b -> (a -> b -> b) -> [a] -> b
listFold forEmpty forCons [] = forEmpty 
listFold forEmpty forCons (first :                                    rest) = 
                           first `forCons` (listFold forEmpty forCons rest)


data Map key value = Map [(key, value)]

map1 :: Map String String
map1 = Map [("Mike", "Sperber"), ("Smadar", "Klopshtok")]

map2 :: Map Pet String
map2 = Map [(Cat, "Katze"), (Dog, "Hund"), (Snake, "Schlange")]

data Optional result =
    Present result
  | Absent 

optionalMap :: (a -> b) -> Optional a -> Optional b
optionalMap f (Present x) = Present (f x)
optionalMap f Absent = Absent 

{-
Zutaten: Typkonstruktor f, Funktion mit Signatur wie map

universalMap identity x == x
universalMap (f . g) x == universalMap f (universalMap g x)
-}

identity x = x

-- Funktion wie .
o :: (b -> c) -> (a -> b) -> (a -> c)
o f g = \ a -> f (g a)

class Functor f where
    universalMap :: (a -> b) -> f a -> f b

instance Functor Optional where
    universalMap = optionalMap

instance Functor [] where
    universalMap = listMap

{-
data Maybe a =    
    Just a
  | Nothing
-}

-- Eintrag in einer Map finden
mapGet :: Eq key => Map key value -> key -> Optional value
--        ^^^^^^ Constraint "keys müssen vergleichbar sein"
mapGet (Map []) key = Absent
mapGet (Map ((key', value') : rest)) key =
    if key == key'
    then Present value'
    else mapGet (Map rest) key

{-
Eq ist eine Typklasse.  (Am ehestens: Java-Interface)

class Eq a where
  (==) :: a -> a -> Bool

-}

{-
Assoziativität
(a + b) + c = a + (b + c)
(a * b) * c = a * (b * c)

Zutaten:
Typ t
Operation: combine :: t -> t -> t

Beispiele:
Images - overlay, beside, above
Shapes - overlap / overlay

Halbgruppe:
Typ t, Operation combine wie oben, Assoziativät

Halbgruppe + neutrales Element n:
combine n x = x
combine x n = x
Monoid
-}

-- grob: Haskell class -> Java interface
--       Haskell instance -> Java class implements

-- Neue Eigenschaft namens "Semigroup"
class Semigroup t where
    -- combine a (combine b c) == combine (combine a b) c
    -- a `combine` (b `combine` c) == (a `combine` b) `combine` c
    combine :: t -> t -> t

instance (Semigroup m, Semigroup n) => Semigroup (m, n) where
  -- m1, m2 :: m
  -- n1, n2 :: n
--  combine (m1, n1) (m2, n2) = (combine m1 m2, combine n1 n2) -- "punktweise"
                          --   combine auf m, combine auf n
  combine x y =
    let (a, b) = x
        (c, d) = y
    in (combine a c, combine b d)
-- combine (m1, n1) (combine (m2, n2) (m3, n3))
-- == combine (m1, n1) (combine m2 m3, combine n2 n3)
-- == (combine m1 (combine m2 m3), combine n1 (combine n2 n3))
-- == (combine (combine m1 m2) m3, (combine (combine n1 n2) n3)) -- Assoziativität in m, n
-- ...
-- == combine (combine (m1, n2) (m2, n2)) (m3, n3)

instance Semigroup [a] where
    combine a b = a ++ b

class Semigroup t => Monoid t where
    -- combine neutral x == x
    -- combine x neutral == x
    neutral :: t

instance (Monoid m, Monoid n) => Monoid (m, n) where
  neutral = (neutral, neutral)

instance Monoid [a] where
    neutral = []

-- braucht wahrscheinlich auch Instanzen für Semigroup
-- instance Monoid m => Monoid (a -> m) where
-- instance Monoid a => Monoid (Maybe a) where

-- instance Semigroup Integer where
--   combine = ???

data Additive = Additive Integer

instance Semigroup Additive where
  combine (Additive n) (Additive m) = Additive (n + m)

data Mult = Mult Integer

instance Semigroup Mult where
  combine (Mult n) (Mult m) = Mult (n * m)

-- data Dual a = Dual a
newtype Dual a = Dual a
-- garantiert kein Laufzeit-Overhead

instance Semigroup m => Semigroup (Dual m) where
  combine (Dual a) (Dual b) = Dual (combine b a)
