{-# OPTIONS_GHC -fwarn-incomplete-patterns #-}
module Intro where

x = 5

-- Zahl verdoppeln
double :: Int -> Int
double x = x * 2

-- Ein Haustier ist eins der folgenden:
-- - Katze
-- - Hund
-- - Schlange
data Pet = Cat | Dog | Snake

-- Ist ein Tier niedlich?
isCute :: Pet -> Bool
isCute Cat = True
isCute Dog = True
isCute Snake = False

-- Ein Gürteltier hat die folgenden Eigenschaften:
-- - Lebendig oder tot
-- - Gewicht
data Liveness = Dead | Alive
 deriving (Show, Eq)

-- algebraischer Datentyp: Gemischte Daten von zusammengesetzten Daten 
data Animal = 
      Dillo { -- Record
                dilloLiveness :: Liveness,
                dilloWeight :: Int
            }
    | Parrot String Int
    deriving Show

dillo1 = Dillo { dilloLiveness = Alive, dilloWeight = 10 } -- lebt noch, 10kg
dillo2 = Dillo Dead 12 -- totes Gürteltier, 12kg

-- Gürteltier überfahren
runOverDillo :: Animal -> Animal
-- runOverDillo dillo =
--    Dillo { dilloLiveness = Dead, dilloWeight = dilloWeight dillo }
-- runOverDillo (Dillo _ weight) =
--     Dillo Dead weight
runOverDillo dillo = dillo { dilloLiveness = Dead }

-- Gürteltier füttern
feedAnimal :: Int -> (Animal -> Animal)
feedAnimal amount (Dillo Alive weight) = Dillo Alive (weight + amount)
-- feedAnimal amount (Dillo Dead weight) = Dillo Dead weight
feedAnimal amount dillo@(Dillo Dead weight) = dillo -- Alias-Pattern
feedAnimal amount (Parrot sentence weight) = Parrot sentence (weight + amount)

-- feedDillo' :: (Integer, Dillo) -> Dillo
-- feedDillo' (amount, Dillo Alive weight) = Dillo Alive (weight + amount)

curry :: ((a, b) -> c) -> (a -> b -> c)
curry f = \ a -> (\ b -> f (a, b))

uncurry :: (a -> b -> c) -> ((a, b) -> c)
-- uncurry f = \ (x, y) -> f x y -- x:: a, y:: b
uncurry f (x, y) = f x y

flip :: (a -> b -> c) -> (b -> a -> c)
flip f = \ b -> \ a -> f a b

feedDillo' = Intro.uncurry feedAnimal
feedDillo'' = Intro.flip feedAnimal
feedDillo''' = Intro.uncurry (Intro.flip (Intro.curry feedDillo'))

-- tupleFlip = Intro.uncurry . Intro.flip . Intro.curry
tupleFlip f = Intro.uncurry (Intro.flip (Intro.curry f))

-- Ein Papagei hat folgende Eigenschaften:
-- - Satz
-- - Gewicht
-- data Parrot = Parrot String Int

parrot1 = Parrot "Der Schatz ist im Silbersee!" 5 -- Piraten-Papagei, 5kg

-- leider nein: data Animal = Dillo | Parrot

data List =
    Empty
  | Cons Int List

-- Elemente einer Liste addieren
listSum :: List -> Int
listSum Empty = 0
listSum (Cons first rest) = first + (listSum rest)

data AList a =
     AEmpty
   | ACons a (AList a)

-- Eingebaut:
list1 = [5] -- 1elementige Liste: 5
list2 = [5, 7] -- 2elementige Liste: 5, 7
list3 = 12:list2 -- 3elementige Liste: 12 5 7

listSum' :: [Int] -> Int
listSum' [] = 0
listSum' (first:rest) = first + (listSum' rest)

listMap :: (a -> b) -> [a] -> [b]
listMap f [] = []
listMap f (x:xs) = (f x) : (listMap f xs)

natsFrom n = n : natsFrom (n + 1)
nats = natsFrom 0

strikeMultiples :: Integer -> [Integer] -> [Integer]
strikeMultiples n [] = []
-- `rem` -- Funktion rem, nur Infix
strikeMultiples n (first:rest) =
    if (first `rem` n) == 0 -- first ist Vielfaches von n
    then strikeMultiples n rest
    else first : (strikeMultiples n rest)

strikeMultiples' n list = filter (\ el -> (el `rem` n) /= 0) list

sieve :: [Integer] -> [Integer]
sieve [] = []
sieve (first:rest) = first : (sieve (strikeMultiples first rest))

-- data Map key value = Map [(key, value)]
-- kein Speicheroverhead für Wrapper-Dings o.ä.
-- geht nur bei 1 Konstruktor, 1 Attribut
data Map key value = Map [(key, value)]

map1 :: Map String String
map1 = Map [("Mike", "Sperber"), ("Angela", "Merkel")]
--     ^^^

mapPut :: key -> value -> Map key value -> Map key value
mapPut key value (Map list) = Map ((key, value) : list)

-- Manchmal da, manchmal nicht:
data Optional a =
    NotThere
  | There a

instance Eq a => Eq (Optional a) where
    (==) NotThere NotThere = True
    (==) (There a1) (There a2) = a1 == a2
    (==) _ _ = False

-- data Maybe a = Nothing | Just a

-- Eintrag in Map suchen
mapGet :: Eq key => -- wenn key vergleichbar ist ...
            key -> Map key value -> Optional value
mapGet key (Map []) = NotThere
mapGet key' (Map ((key, value):rest)) = 
    if key' == key
    then There value
    else mapGet key' (Map rest)


-- Typklassen

instance Eq Animal where
   -- (==) :: Animal -> Animal -> Bool 
   (==) (Dillo liveness1 weight1) (Dillo liveness2 weight2) =
            (liveness1 == liveness2) && (weight1 == weight2)
   (==) (Parrot sentence1 weight1) (Parrot sentence2 weight2) =
            (sentence1 == sentence2) && (weight1 == weight2)
   (==) (Parrot _ _) (Dillo _ _) = False
   (==) (Dillo _ _) (Parrot _ _) = False

-- (+) :: Integer -> Integer -> Integer 
-- (a + b) + c = a + (b + c) -- Assoziativgesetz
-- (*) :: Integer -> Integer -> Integer
-- (a * b) * c = a * (b * c)
-- (++) :: [a] -> [a] -> [a]
-- (l1 ++ l2) ++ l3 = l1 ++ (l2 ++ l3)
-- Halbgruppe / Semigroup: Menge + Kombinator + Assoziativgesetz

class Semigroup a where
    combine :: a -> a -> a
    -- (a `combine` b) `combine` c = a `combine (b `combine` c)

instance Intro.Semigroup [a] where
    combine l1 l2 = l1 ++ l2

instance Intro.Semigroup Integer where
    combine = (+)

instance (Intro.Semigroup a, Intro.Semigroup b) => Intro.Semigroup (a, b) where
    combine (a1, b1) (a2, b2) =
        (combine a1  a2, combine b1 b2)

-- neutrales Element
-- n + 0 = 0 + n = n
-- n * 1 = 1 * n = n
-- [] ++ l = l ++ [] = n

-- Halbgruppe + neutrales Element = Monoid
class Intro.Semigroup a => Monoid a where
    neutral :: a

instance Intro.Monoid [a] where
    neutral = []

instance Intro.Monoid Integer where
    neutral = 0

-- listMap ::     (a -> b) -> [a]        -> [b]
-- listMap ::     (a -> b) -> List a     -> List b
-- optionalMap :: (a -> b) -> Optional a -> Optional b

class Functor constructor where
    mmap :: (a -> b) -> constructor a -> constructor b

instance Intro.Functor [] where
    mmap = map 

instance Intro.Functor Optional where
    mmap = optionalMap