{-# LANGUAGE InstanceSigs #-}
module Intro where

import Prelude hiding (Semigroup, Monoid)

x :: Integer
x = 5

y :: Integer
y = x * 2

-- Zeilenkommentar
-- >>> x + y
-- 15

double :: Integer -> Integer
-- double = \ x -> x * 2
double x = x * 2

-- >>> double 7
-- 14

foo x y =
 let z = x + y
     a = z * 2 
    in z * 4 + a

-- Ein Haustier ist eins der folgenden:
-- - Hund -ODER-
-- - Katze -ODER-
-- - Schlange

-- algebraischer Datentyp / Aufzählung
data Pet =
    Dog 
  | Cat
  | Snake
  deriving Show

instance Eq Pet where
  (==) :: Pet -> Pet -> Bool
  (==) Dog Dog = True
  (==) Cat Cat = True
  (==) Snake Snake = True
  (==) _ _ = False

-- Ist Haustier niedlich?
isCute :: Pet -> Bool
-- eine Gleichung pro Fall
isCute Dog = True
isCute Cat = True
isCute Snake = False

-- >>> isCute Dog
-- True

-- >>> isCute Snake
-- False

-- Gürteltier hat folgende Eigenschaften:
-- - lebendig -ODER- tot -UND-
-- - Gewicht
-- zusammengesetzte Daten

-- Typsynonym
type Weight = Integer

data Liveness = Alive | Dead 
  deriving (Eq, Show)

{-
-- Record
data Dillo =
    MkDillo { dilloLiveness :: Liveness,
              dilloWeight :: Weight }
    deriving Show

-- lebendiges Gürteltier, 10kg
dillo1 :: Dillo
dillo1 = MkDillo { dilloLiveness = Alive, dilloWeight = 10 }

dillo2 :: Dillo
-- dillo2 = MkDillo { dilloLiveness = Dead, dilloWeight = 8 }
dillo2 = MkDillo Dead 8

-- >>> dilloLiveness dillo1
-- Alive
-- >>> dilloWeight dillo2
-- 8

-- Gürteltier überfahren
runOverDillo :: Dillo -> Dillo
-- runOverDillo dillo = MkDillo { dilloLiveness = Dead, 
--                               dilloWeight = dilloWeight dillo }
-- runOverDillo dillo = MkDillo Dead (dilloWeight dillo)
-- runOverDillo (MkDillo { dilloLiveness = l, dilloWeight = w }) =
--    MkDillo Dead w
--- runOverDillo (MkDillo { dilloWeight = w }) =
--    MkDillo Dead w
-- runOverDillo (MkDillo _ weight) = MkDillo Dead weight -- - "don't care"
runOverDillo dillo = dillo { dilloLiveness = Dead } -- Kopie von dillo, bis auf ... "functional update"


-- >>> runOverDillo dillo1
-- MkDillo {dilloLiveness = Dead, dilloWeight = 10}
-}

-- Ein Tier (auf dem texanischen Highway) ist eins der folgenden:
-- - Gürteltier -ODER-
-- - Papagei
-- Fallunterscheidung

-- Ein Papagei hat folgende Eigenschaften:
-- - Satz
-- - Gewicht
data Animal =
     MkDillo { dilloLiveness :: Liveness,
               dilloWeight :: Weight }
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
runOverAnimal (MkDillo _liveness weight) =
  MkDillo Dead weight
runOverAnimal (MkParrot _sentence weight) =
  MkParrot "" weight

-- Datenmodell für die Karten des französischen Blatts

-- Yaron Minsky: "Make illegal states unrepresentable."

bar :: Double -> Double -> Double
-- bar x y = (x + y)/2
bar = \ x -> \ y -> (x + y)/2

feedAnimal :: Animal -> (Weight -> Animal)
feedAnimal dillo@(MkDillo liveness weight) amount = -- alias pattern
  case liveness of
    Alive -> MkDillo liveness (weight + amount)
    Dead -> dillo -- MkDillo liveness weight
feedAnimal (MkParrot sentence weight) amount = 
  MkParrot sentence (weight + amount)

-- eingebaut als "flip"
-- swap :: (Animal -> Weight -> Animal) -> (Weight -> Animal -> Animal)
swap :: (a -> b -> c) -> (b -> a -> c) -- Typvariablen
-- swap f = \ b -> \ a -> f a b
swap f b a = f a b

-- feedAnimal' :: Weight -> Animal -> Animal
-- feedAnimal' = swap feedAnimal

feedAnimal' :: (Animal, Weight) -> Animal
feedAnimal'(dillo@(MkDillo liveness weight), amount) =
  -- alias pattern
  case liveness of
    Alive -> MkDillo liveness (weight + amount)
    Dead -> dillo -- MkDillo liveness weight
feedAnimal'(MkParrot sentence weight, amount) =
  MkParrot sentence (weight + amount)

-- Haskell Curry
-- Moses Schönfinkel

-- "entschönfinkeln"
-- eingebaut als "uncurry"
tuplify :: (a -> b -> c) -> ((a, b) -> c)
-- tuplify f = \ (a, b) -> f a b
tuplify f (a, b) = f a b

-- "schönfinkeln"
-- eingebaut als "curry"
untuplify :: ((a, b) -> c) -> (a -> b -> c)
-- untuplify f = \a -> \b -> f (a, b)
untuplify f a b = f (a, b)

-- Funktionskomposition
-- eingebaut als . (Infix)
o :: (b -> c) -> (a -> b) -> (a -> c)
o f g = \ a -> f (g a)

feedAndThenRunOver :: Animal -> Animal
feedAndThenRunOver = runOverAnimal . flip feedAnimal 1

-- Eine (geometrische) Figur ist eins der folgenden:
-- - Kreis -ODER-
-- - Quadrat -ODER-
-- - Überlagerung zweier geometrischer Figuren

-- 1. Datentype(en)
-- 2. Funktion, die bei einem Punkt feststellt, 
--    ob er innerhalb oder außerhalb einer geomtrischen Figur liegt

data Point = MkPoint Double Double

point1 = MkPoint 1 1
point2 = MkPoint 3 3
point3 = MkPoint 10 4

data Shape
  = MkCircle {center :: Point, radius :: Double}
  | MkSquare {leftBottom :: Point, sideLength :: Double}
  | MkOverlap {shape1 :: Shape, shape2 :: Shape}

circle1 = MkCircle (MkPoint 2 2) 2.0

square1 = MkSquare (MkPoint 3 3) 4.0

within :: Shape -> Point -> Bool
within (MkCircle (MkPoint centerX centerY) radius) (MkPoint x y) =
  let distanceX = (x - centerX) ^ 2
      distanceY = (y - centerY) ^ 2
      difference = sqrt (distanceX + distanceY)
   in difference <= radius
within (MkSquare (MkPoint squareX squareY) sideLength) (MkPoint x y) =
  let rightTopX = squareX + sideLength
      rightTopY = squareY + sideLength
   in ((x >= squareX) && (x <= rightTopX))
        && ((y >= squareY) && (y <= rightTopY))
within (MkOverlap shape1 shape2) point =
  within shape1 point || within shape2 point


-- Eine Liste ist eins der folgenden:
-- - die leere Liste
-- - eine Cons-Liste aus erstem Element und Rest-Liste

data ListOf a =
    EmptyList
  | Cons a (ListOf a)

-- eingebaut:
-- [] leere Liste
-- first:rest  : = Cons

list1 :: [Integer]
list1 = 5 : []

list2 :: [Integer]
list2 = 2 : (5 : [])

list3 :: [Integer]
list3 = [2, 5, 4]

list4 :: [Integer]
list4 = 3 : list3

listSum :: [Integer] -> Integer
listSum [] = 0
listSum (first:rest) = first + (listSum rest)

listFilter :: (a -> Bool) -> [a] -> [a]
listFilter p [] = []
listFilter p (x:xs) =
  if p x 
  then x : (listFilter p xs)
  else listFilter p xs

type List a = [a]

listMap :: (a -> b) ->    List a      -> List b
listMap f [] = []
listMap f (x:xs) = f x : (listMap f xs)

optionalMap :: (a -> b) -> Optional a -> Optional b
optionalMap f Null = Null
optionalMap f (Result a) = Result (f a)

-- Liste aller Karten des französischen Blatts

data Suit = Diamonds | Clubs | Spades | Hearts
  deriving (Show, Eq, Enum)

-- | Liste aller Farben
-- >>> length allSuits
-- 4

allSuits :: [Suit]
allSuits = [Spades, Hearts, Diamonds, Clubs]

data Rank
  = Two
  | Three
  | Four
  | Five
  | Six
  | Seven
  | Eight
  | Nine
  | Ten
  | Jack
  | Queen
  | King
  | Ace
  deriving (Show, Eq, Ord, Enum)

-- | Liste aller Werte
-- >>> length allRanks
-- 13
allRanks :: [Rank]
allRanks =
  [ Two,
    Three,
    Four,
    Five,
    Six,
    Seven,
    Eight,
    Nine,
    Ten,
    Jack,
    Queen,
    King,
    Ace
  ]

-- | Spielkarte
data Card = Card {suit :: Suit, rank :: Rank}
  deriving Show

-- allCards = concat (listMap allCardsWithSuit allSuits)
allCards :: [Card]
-- allCards = concat (listMap (\suit -> listMap (\rank -> (Card suit) rank) allRanks) allSuits)
allCards = concat (listMap (\suit -> listMap (Card suit) allRanks) allSuits)


all :: (a -> b -> c) -> [a] -> [b] -> [c]
all constructor list1 list2 = concat (listMap (\x2 -> listMap (\x1 -> constructor x2 x1) list2) list1)

cartesianProduct :: [a] -> [b] -> [(a, b)]
cartesianProduct list1 list2 = concat (listMap (\x2 -> listMap (\x1 -> (x1, x2)) list1) list2)

allCards' = listMap (uncurry Card) (cartesianProduct allSuits allRanks)

-- >>> length allCards
-- 52

allCardsWithSuit :: Suit -> [Card]
allCardsWithSuit suit = listMap (\rank -> Card suit rank) allRanks

-- F#: |>

(|>) = flip (.)

feedAndThenRunOver' = flip feedAnimal 1 |> runOverAnimal

-- Index eines Elements in einer Liste bestimmen

-- >>> listIndex 5 [1, 3, 7, 5, 12]
-- Result 3

-- >>> listIndex Cat [Dog, Snake, Cat, Snake]
-- Result 2


data Optional a =
  Null | Result a
  deriving Show

-- >>> :info Applicative
-- type Applicative :: (* -> *) -> Constraint
-- class Functor f => Applicative f where
--   pure :: a -> f a
--   (<*>) ::  f (a ->   b) -> f a -> f b

--   fmap  ::    (a ->   b) -> f a -> f b
-- flip (>>=) :: (a -> f b) -> f a -> f b

--   (>>=) :: f a -> (a -> f b) -> f b

fmap' f x = (pure f) <*> x

data Validation a =
    Valid a
  | Invalid [String] -- Liste von Fehlermeldungen

-- Übung: Functor, Applicative für Validation

instance Functor Validation where
  fmap f (Valid a) = Valid (f a)
  fmap f (Invalid errors) = Invalid errors

instance Applicative Validation where
  pure = Valid
  (<*>) (Valid fa) (Valid a) = Valid (f a)
  (<*>) (Valid _) (Invalid errors) = Invalid errors
  (<*>) (Invalid errors) (Valid _) = Invalid errors
  (<*>) (Invalid errors1) (Invalid errors2) = Invalid (errors1 + errors2)

      

instance Applicative Optional where
  pure :: a -> Optional a
  pure = Result
  (<*>) :: Optional (a -> b) -> Optional a -> Optional b
  (<*>) Null Null = Null
  (<*>) Null (Result a) = Null
  (<*>) (Result fa) Null = Null
  (<*>) (Result fa) (Result a) = Result (fa a)

instance Monad Optional where
  return = Result
  (>>=) :: Optional a -> (a -> Optional b) -> Optional b
  (>>=) Null next = Null
  (>>=) (Result a) next = next a

newtype UserName = MkUserName String
newtype UserAge = MkUserAge Integer

data User = MkUser { userName:: UserName, userAge :: UserAge }

-- fmap :: (a -> b) -> f a -> f b
fmap2 :: Applicative f => (a -> b -> c) -> f a -> f b -> f c
fmap2 f fa fb = 
   -- (pure f) <*> fa <*> fb
   -- fmap f fa <*> fb
   f <$> fa <*> fb

-- :type <$> 

fmap3 :: Applicative f => (a -> b -> c -> d) -> f a -> f b -> f c -> f d
fmap3 f fa fb fc =
  (pure f) <*> fa <*> fb <*> fc

validateUserName letters =
  if length letters > 50
  then Null
  else Result (MkUserName letters)

validateUserAge years =
  if years < 0
  then Null
  else Result (MkUserAge years)

makeUserFromForm :: String -> Integer -> Optional User
makeUserFromForm letters years = 
  fmap2 MkUser (validateUserName letters) (validateUserAge years)
{-  do name <- validateUserName letters
     age <- validateUserAge years
     return (MkUser name age)
     -}
  {-
  case validateUserName letters of
    Result name ->
      case validateUserAge years of
        Result age -> Result (MkUser name age)
        Null -> Null
    Null -> Null
  -}



-- eingebaut:
-- data Maybe a = Nothing | Just a

listIndex :: Eq a => a -> [a] -> Optional Integer
listIndex a [] = Null
listIndex a (first:rest) =
  if a == first   
  then Result 0
  else fmap (+1) -- (\index -> index + 1)
            (listIndex a rest)
    
    {- case listIndex a rest of 
         Null -> Null
         Result index -> Result (index+1)
      -}

-- class ... Typklasse ... "Interface"
-- instance ... Instanz ... "Implementierung"

-- >>> :info Eq
-- type Eq :: * -> Constraint
-- class Eq a where
--   (==) :: a -> a -> Bool
--   (/=) :: a -> a -> Bool
--   {-# MINIMAL (==) | (/=) #-}
--   	-- Defined in ‘GHC.Classes’

-- >>> :info Show
-- type Show :: * -> Constraint
-- class Show a where
--   showsPrec :: Int -> a -> ShowS
--   show :: a -> String
--   showList :: [a] -> ShowS
--   {-# MINIMAL showsPrec | show #-}
--   	-- Defined in ‘GHC.Show’

-- >>> :info Ord
-- type Ord :: * -> Constraint
-- class Eq a => Ord a where
--   compare :: a -> a -> Ordering
--   (<) :: a -> a -> Bool
--   (<=) :: a -> a -> Bool
--   (>) :: a -> a -> Bool
--   (>=) :: a -> a -> Bool
--   max :: a -> a -> a
--   min :: a -> a -> a
--   {-# MINIMAL compare | (<=) #-}

-- Algebra

-- 1. Menge(n) / Typ: T
-- 2. Operation(en)
-- 3. Gesetze / Gleichungen

-- 1. Integer
-- 2. + :: Integer -> Integer -> Integer
-- 3. (a + b) + c = a + (b + c)         Assoziativgesetz

-- Halbgruppe
-- 1. T
-- 2. op :: T -> T -> T
-- 3. op (op a b) c = op a (op b c)

class Semigroup t where
  -- op (op a b) c == op a (op b c)
  op :: t -> t -> t

instance Semigroup [a] where
  op :: [a] -> [a] -> [a]
  op = (++)

instance Semigroup Shape where
  op :: Shape -> Shape -> Shape
  op = MkOverlap
  
-- Halbgruppe,
-- darin neutrales Element neutral
-- op neutral x == op x neutral == x

-- Halbgruppe mit neutralem Element: Monoid

class Semigroup t => Monoid t where
  neutral :: t

instance Monoid [a] where
  neutral :: [a]
  neutral = []

monoidConcat :: Monoid a => [a] -> a
monoidConcat [] = neutral
monoidConcat (x:xs) = 
  op x (monoidConcat xs)

-- >>> monoidConcat [[1,2,3], [4,5,6]]
-- [1,2,3,4,5,6]

instance (Semigroup a, Semigroup b) => Semigroup (a, b) where
  op (a1, b1) (a2, b2) = (op a1 a2, op b1 b2)

instance (Monoid a, Monoid b) => Monoid (a, b) where
  neutral = (neutral, neutral)

-- Aufgabe: Monoid für Optional a

instance Semigroup a => Semigroup (Optional a) where
  op Null Null = Null
  op (Result a1) Null = Result a1
  op Null (Result a2) = Result a2
  op (Result a1) (Result a2) = Result (op a1 a2)

instance Semigroup a => Monoid (Optional a) where
  neutral = Null

-- Funktor: Typ*konstruktor*, Typ mit Typparameter
-- bzw. "Funktion auf Typebene"
-- C#: List<A>
{-
interface Functor<F> {
  F<B> Map(func<A, B> f, F<A> thing);
}

-}

-- >>> :info Functor
-- type Functor :: (* -> *) -> Constraint
-- class Functor f where
--   fmap :: (a -> b) -> f a -> f b

instance Functor Optional where
  fmap :: (a -> b) -> Optional a -> Optional b
  fmap = optionalMap

-- instance Functor [] where
--   fmap = listMap
