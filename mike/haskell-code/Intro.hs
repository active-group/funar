{-# LANGUAGE InstanceSigs #-}
module Intro where

import Prelude hiding (Semigroup, Monoid)

x :: Integer
x = 23 + 42 * 2

y :: Integer
y = x * 2

-- Zahl verdoppeln
double :: Integer -> Integer
-- >>> double 5 
-- 10

-- double = \ x -> x * 2
double x = x * 2 -- syntaktischer Zucker

-- Abseitsregel: Wenn ein Konstrukt über mehrere Zeilen geht,
-- müssen die Folgezeilen gegenüber der ersten Zeile eingerückt
-- sein.

-- Haustier ist eins der folgenden:
-- - Hund -ODER-
-- - Katze -ODER-
-- - Schlange
data Pet = -- data: neuer Datentyp
    Dog -- "Konstruktor"
  | Cat
  | Snake
  deriving Show -- Zauberspruch

instance Eq Pet where
    (==) :: Pet -> Pet -> Bool
    (==) Cat Cat = True
    (==) Dog Dog = True
    (==) Snake Snake = True
    (==) _ _ = False
--    (==) Cat Dog = False
--    (==) Dog Cat = False
--    (==) Cat Snake = False
--    (==) Snake Cat = False
--    (==) Snake Dog = False
--    (==) Dog Snake = False

-- Ist Haustier niedlich?
isCute :: Pet -> Bool
-- 1 Gleichung pro Fall
-- Schablone:
-- isCute Dog = undefined
-- isCute Cat = undefined
-- isCute Snake = undefined

-- >>> isCute Dog
-- True
-- >>> isCute Snake
-- False

-- Pattern-Matching:
isCute Dog = True
isCute Cat = True
isCute Snake = False
-- isCute pet =
--    case pet of
--        Dog -> True
--        Cat -> True
--        Snake -> False

-- Gürteltier hat folgende Eigenschaften:
-- - lebendig ODER tot
-- - Gewicht

data Liveness = Alive | Dead
  deriving (Show, Eq)

type Weight = Integer -- Typsynonym

-- Tier auf dem texanischen Highway:
-- - Gürteltier -ODER-
-- - Schlange

{-
data Dillo = 
    MkDillo { dilloLiveness :: Liveness, -- MkDillo: Konstruktor, dilloLiveness ... Selektoren
              dilloWeight :: Weight }
    deriving Show

-- lebendiges Gürteltier 10kg
dillo1 :: Dillo
dillo1 = MkDillo { dilloLiveness = Alive, dilloWeight = 10}

-- totes Gürteltier, 8kg
dillo2 :: Dillo
dillo2 = MkDillo Dead 8

-- >>> dilloLiveness dillo1
-- Alive

-- >>> dilloWeight dillo2
-- 8

-- Gürteltier überfahren
runOverDillo :: Dillo -> Dillo

-- >>> runOverDillo dillo1
-- MkDillo {dilloLiveness = Dead, dilloWeight = 10}

-- runOverDillo dillo =
--    MkDillo {dilloLiveness = Dead, dilloWeight = dilloWeight dillo }
-- runOverDillo dillo = MkDillo Dead (dilloWeight dillo)
-- runOverDillo (MkDillo { dilloLiveness = l, dilloWeight = w }) = -- Pattern
--    MkDillo { dilloLiveness = Dead, dilloWeight = w }
-- runOverDillo (MkDillo _liveness weight) = MkDillo Dead weight -- _: "don't care"
runOverDillo dillo = dillo { dilloLiveness = Dead } -- "functional update", Kopie bis auf ...

-- nur 1stellige Funktionen in Haskell
feedDillo :: Dillo -> Weight -> Dillo
feedDillo dillo amount =
    case dilloLiveness dillo of
        Alive -> dillo { dilloWeight = dilloWeight dillo + amount }
        Dead -> dillo

feedDillo' :: Weight -> Dillo -> Dillo
feedDillo' amount dillo =
    case dilloLiveness dillo of
      Alive -> dillo {dilloWeight = dilloWeight dillo + amount}
      Dead -> dillo

-- Eingaben vertauschen
-- swap :: (Dillo -> Weight -> Dillo) -> (Weight -> Dillo -> Dillo)

-- >>> (swap feedDillo) 5 dillo1
-- MkDillo {dilloLiveness = Alive, dilloWeight = 15}

swap :: (a -> b -> c) -> (b -> a -> c)
-- swap f = \ weight -> \ dillo -> f dillo weight
-- swap f = \b -> \a -> f a b
swap f b a = f a b
-- eingebaut flip

-- Tupel: Ad-hoc zusammengesetzte Daten

-- >>> feedDillo''(dillo1, 5)
-- MkDillo {dilloLiveness = Alive, dilloWeight = 15}

feedDillo'' :: (Dillo, Weight) -> Dillo
feedDillo''(dillo, amount) =
  case dilloLiveness dillo of
    Alive -> dillo {dilloWeight = dilloWeight dillo + amount}
    Dead -> dillo

-- Haskell Curry -> curry, curryfizieren
-- Moses Schönfinkel -> schönfinkeln

-- eingebaut: curry, uncurry

-- a, b, c: Typvariablen (kleingeschrieben)
entschönfinkeln :: (a -> b -> c) -> ((a, b) -> c)

-- >>> (tuplify feedDillo) (dillo1, 5)
-- MkDillo {dilloLiveness = Alive, dilloWeight = 15}

-- tuplify f = \ (a, b) -> f a b
entschönfinkeln f (a, b) = f a b

schönfinkeln :: ((a, b) -> c) -> a -> b -> c
schönfinkeln f a b = f (a, b)
-}

type Length = Integer
type Thickness = Integer


data Animal =
    MkDillo { dilloLiveness :: Liveness, dilloWeight :: Weight}
  | MkSnake Length Thickness -- nur verwendbar mit Pattern-Matching, abgekürzten Notation
  deriving (Show, Eq)

dillo1 :: Animal
dillo1 = MkDillo { dilloLiveness = Alive, dilloWeight = 10 }

dillo2 :: Animal
dillo2 = MkDillo Dead 8

snake1 :: Animal
snake1 = MkSnake 300 10

runOverAnimal :: Animal -> Animal

-- >>> runOverAnimal dillo1
-- MkDillo {dilloLiveness = Dead, dilloWeight = 10}

-- >>> runOverAnimal snake1
-- MkSnake 300 0

-- jede Gleichung muß Konstruktor erwähnen
runOverAnimal (MkDillo _ weight) = MkDillo Dead weight
runOverAnimal (MkSnake length _) = MkSnake length 0

-- Geometrische Figur ("shape") ist eins der folgenden:
-- -- Kreis -ODER-
-- -- Quadrat -ODER-
-- -- Überlagerung zweier geometrischer Figuren

-- Aufgabe:
-- 1. Datenanalyse, Repräsentation
-- 2. Funktion, die für einen Punkt feststellt, ob er innerhalb einer geometrischen Figur liegt

type Point = (Double, Double)

point1 :: Point
point1 = (1, 1)

point2 :: Point
point2 = (3, 3)

point3 :: Point
point3 = (10, 4)

-- algebraischer Datentyp
data Shape
  = MkCircle {center :: Point, radius :: Double}
  | MkSquare {leftBottom :: Point, sideLength :: Double}
  | MkOverlap {shape1 :: Shape, shape2 :: Shape} -- Kombinator

circle1 = MkCircle (2, 2) 2.0

square1 = MkSquare (3, 3) 4.0

within :: Shape -> Point -> Bool
within (MkCircle (centerX, centerY) radius) (x, y) =
  let distanceX = (x - centerX) ^ 2
      distanceY = (y - centerY) ^ 2
      difference = sqrt (distanceX + distanceY)
   in difference <= radius
within (MkSquare (squareX, squareY) sideLength) (x, y) =
  let rightTopX = squareX + sideLength
      rightTopY = squareY + sideLength
   in ((x >= squareX) && (x <= rightTopX))
        && ((y >= squareY) && (y <= rightTopY))
within (MkOverlap shape1 shape2) point =
  within shape1 point || within shape2 point

-- Liste ist eins der folgenden:
-- - die leere Liste                                 []
-- - Cons-Liste aus erstem Element und Rest-Liste    first : rest
--                                                         ^ "cons", Infix

-- data ListOfIntegers =
--    EmptyList
--  | Cons Integer ListOfIntegers

data ListOf a =
    EmptyList
  | Cons a (ListOf a)

list1 :: [Integer]
list1 = 5 : []

list2 :: [Integer]
list2 = 2 : 5 : []

list3 :: [Integer]
list3 = [7, 2, 5] -- syntaktischer Zucker

list4 :: [Integer]
list4 = 8 : list3

listSum :: [Integer] -> Integer

-- >>> listSum list4
-- 22

listSum [] = 0
listSum (first:rest) = first + (listSum rest)

type List a = [a]

listMap :: (a -> b) -> List a -> List b
listMap f [] = []
listMap f (x:xs) = f x : listMap f xs

data Optional a =
    Null
  | Result a
  deriving Show

optionalMap :: (a -> b) -> Optional a -> Optional b
optionalMap f Null = Null
optionalMap f (Result a) = Result (f a)

-- Eq a: Constraint "a ist vergleichbar bzw. unterstützt die ==-Operation"

-- Eq: Typklasse ~ Interface, Instanz ~ Klasse/Implementierung einer Typklasse

-- >>> :info Eq
-- type Eq :: * -> Constraint
-- class Eq a where
--   (==) :: a -> a -> Bool  -- Methode

-- Index eines Elements in einer Liste finden
listIndex :: Eq a => [a] -> a -> Optional Integer

-- >>> listIndex [1, 5, 7, 2] 7
-- Result 2

-- >>> listIndex [Dog, Snake, Cat, Snake, Dog] Cat
-- Result 2

listIndex [] element = Null
listIndex (x:xs) element =
    if element == x
    then Result 0
    else
      optionalMap (+ 1) -- das gleiche (\ index -> index + 1)
                  (listIndex xs element)
      {-
        case listIndex xs element of
            Null -> Null
            Result index -> Result (index + 1)
-}

-- >>> :info Functor
-- type Functor :: (* -> *) -> Constraint
-- class Functor f where
--   fmap :: (a -> b) -> f a -> f b
-- => braucht Typ mit Typparameter

instance Functor Optional where
  fmap :: (a -> b) -> Optional a -> Optional b
  fmap = optionalMap

-- >>> :info Show
-- type Show :: * -> Constraint
-- class Show a where
--   show :: a -> String

-- Algebra

-- algebraische Struktur:
-- - Menge / Typ
-- - Operationen
-- - Gesetze/Gleichungen



-- Gruppe
-- "neutrales Element"

-- 1 + 1 == 2

-- Halbgruppe
-- Typ a
-- op :: a -> a -> a
-- Assoziativität (Eigenschaft von op)
-- op x (op y z) == op (op x y) z
-- x + (y + z) == (x + y) + z

class Semigroup a where
  -- op x (op y z) == op (op x y) z
  op :: a -> a -> a

instance Semigroup [a] where
    op :: [a] -> [a] -> [a]
    op x y = x ++ y

instance Semigroup Shape where
    op :: Shape -> Shape -> Shape
    op = MkOverlap

-- Halbgruppe mit neutralem Element: Monoid
class Semigroup a => Monoid a where -- jeder Monoid muß auch eine Halbgruppe
    neutral :: a

instance Monoid [a] where
    neutral :: [a]
    neutral = []

instance (Semigroup a, Semigroup b) => Semigroup (a, b) where
    op :: (a, b) -> (a, b) -> (a, b)
    op (a1, b1) (a2, b2) = (op a1 a2, op b1 b2)

instance (Monoid a, Monoid b) => Monoid (a, b) where
    neutral :: (a, b)
    neutral = (neutral, neutral)

-- Übung:
instance Semigroup a => Semigroup (Optional a) where
    op :: Optional a -> Optional a -> Optional a
    op Null o = o
    op o Null = o
    op (Result a1) (Result a2) = Result (op a1 a2)

instance Semigroup a => Monoid (Optional a) where
    neutral :: Optional a
    neutral = Null


data AndBool = AndBool Bool
  deriving Show

instance Semigroup AndBool where
  op :: AndBool -> AndBool -> AndBool
  op (AndBool b1) (AndBool b2) = AndBool (b1 && b2)

-- >>> op (Result (AndBool True)) (Result (AndBool False))
-- Result (AndBool False)

-- >>> op (Result [1,2,3]) Null
-- Result [1,2,3]
-- >>> op (Result [1,2,3]) (Result [4,5,6])
-- Result [1,2,3,4,5,6]

-- >>> op [1,2,3] [4,5,6]
-- [1,2,3,4,5,6]

-- >>> op [1,2,3] neutral
-- [1,2,3]

-- >>> op ([1,2,3], [4,5,6]) ([4,5,6], [1,2,3])
-- ([1,2,3,4,5,6],[4,5,6,1,2,3])

-- Typ a
-- neutral :: a
-- op :: a -> a -> a

-- Für alle x:
-- op neutral x == op x neutral == x

-- 0 + x == x + 0 == x -- neutrales Element von +
-- 1 * x == x * 1 == x -- neutrales Element von *

-- wichtige algebraische Strukturen:
-- Halbgruppe -> Monoid
-- Funktor -> applikativer Funktor -> Monade

-- "Make illegal states unrepresentable."
-- => Wenn ich ein Objekt vom Typ T habe, dann ist es auch korrekt.
-- => "Parse don't validate."

data Validation a = -- wie Optional
    Valid a
  | Invalid [String] -- Fehlermeldungen
  deriving Show

instance Functor Validation where

instance Applicative Validation where

newtype EMail = MkEMail String
  deriving Show

newtype Age = MkAge Integer
  deriving Show

data User = MkUser EMail Age
  deriving Show

---mike = MkUser "§%$()%$§%/)§$%()$" (-100)

validateAge :: Integer -> Optional Age
validateAge n =
  if n < 0
  then Null
  else if n > 120
  then Null
  else Result (MkAge n)

validateEMail :: String -> Optional EMail
validateEMail s =
  case listIndex s '@' of
    Null -> Null
    Result _ -> Result (MkEMail s)

makeUser :: String -> Integer -> Optional User
makeUser s n =
  -- fmap2 MkUser (validateEMail s) (validateAge n)
  MkUser <$> validateEMail s <*> validateAge n
  {-
  case validateEMail s of
    Null -> Null
    Result email ->
      case validateAge n of
        Null -> Null
        Result age -> Result (MkUser email age)
-}
-- >>> :info Applicative
-- type Applicative :: (* -> *) -> Constraint
-- class Functor f => Applicative f where
--   pure :: a -> f a
--   (<*>) :: f (a -> b) -> f a -> f b

instance Applicative Optional where
  pure :: a -> Optional a
  pure = Result

  (<*>) :: Optional (a -> b) -> Optional a -> Optional b
  (<*>) Null Null = Null
  (<*>) (Result oab) Null = Null
  (<*>) Null (Result oa) = Null
  (<*>) (Result oab) (Result oa) = Result (oab oa)

-- optionalMap2 :: (a -> b -> c) -> Optional a -> Optional b -> Optional c
fmap2 :: Applicative f => (a -> b -> c) -> f a -> f b -> f c
fmap2 fabc oa ob =
  -- (pure fabc) <*> oa <*> ob
  fabc <$> oa <*> ob
  --   ^^^ Synonym für fmap

instance Functor Validation where
  fmap :: (a -> b) -> Validation a -> Validation b
  fmap f (Valid a) = Valid (f a)
  fmap f (Invalid errors) = Invalid errors

instance Applicative Validation where
  pure :: a -> Validation a
  pure = Valid
  (<*>) :: Validation (a -> b) -> Validation a -> Validation b
  (<*>) (Invalid errors1) (Invalid errors2) = Invalid (errors1 ++ errors2)
  (<*>) (Invalid errors) (Valid a) = Invalid errors
  (<*>) (Valid f) (Invalid errors) = Invalid errors
  (<*>) (Valid f) (Valid a) = Valid (f a)

-- fmap ::        (a ->   b) -> f a -> f b
-- (<*>) ::     f (a ->   b) -> f a -> f b
-- flip (>>=) ::  (a -> f b) -> f a -> f b