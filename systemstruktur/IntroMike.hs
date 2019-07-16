module Intro where

x = 5

double :: Integer -> Integer
double x = x * 2

-- Aggregatzustand ist eins der folgenden:
-- fest
-- flüssig
-- gasförmig
data State = Solid | Liquid | Gas
 deriving Show

-- Aggregatzustand von Wasser ermitteln
waterState :: Double -> State
waterState temp =
    if temp <= 0
    then Solid
    else if temp <= 100
    then Liquid
    else Gas

waterState' :: Double -> State
waterState' temp
  | temp <= 0   = Solid
  | temp <= 100 = Liquid
  | otherwise   = Gas

-- typische Temperatur pro Aggregatzustand
typicalTemperature :: State -> Double
-- 3 Fälle - 3 Gleichungen
typicalTemperature Solid  = -20 
typicalTemperature Liquid = 18
typicalTemperature Gas    = 101

-- Ein Gürteltier hat folgende Eigenschaften:
-- lebendig oder tot
-- Gewicht
data Liveness = Dead | Alive
  deriving Show

{-
data Dillo = Dillo { dilloLiveness :: Liveness,
                     dilloWeight   :: Integer }
  deriving Show

d1 = Dillo Dead 20 -- totes Gürteltier, 20kg
d2 = Dillo Alive 30 -- lebenes Gürteltier, 30kg

-- Gürteltier überfahren
runOverDillo :: Dillo -> Dillo
-- runOverDillo d =
--    Dillo Dead (dilloWeight d)
runOverDillo (Dillo _ w) = Dillo Dead w

-- Gürteltier füttern
-- alle Funktionen sind einstellig!
-- "currifizierte Funktionen" (Haskell Curry)
feedDillo :: Dillo -> (Integer -> Dillo)
feedDillo (Dillo Alive weight) amount = Dillo Alive (weight + amount) 
-- feedDillo (Dillo Dead weight) amount  = Dillo Dead weight
-- feedDillo dillo@(Dillo Dead weight) amount  = dillo
feedDillo d _ = d

-- eingebaut als flip
swap :: (t1 -> t2 -> t3) -> t2 -> t1 -> t3
-- swap f y x = f x y
swap f = \ y x -> f x y

feedDillo' = swap feedDillo

feedDillo'' (Dillo Alive weight, amount) = Dillo Alive (weight + amount) 
feedDillo'' (d, _) = d

-- eingebaut als uncurry
tupelize :: (a -> b -> c) -> ((a, b) -> c)
tupelize f = \ (x, y) -> f x y

-- eingebaut als curry
detupelize :: ((a, b) -> c) -> (a -> b -> c)
detupelize f = \ x y -> f (x, y)

-- Ein Papagei hat folgende Eigenschaften:
-- - Satz
-- - Gewicht
data Parrot = Parrot String Integer
  deriving Show

p1 = Parrot "Mike ist doof!" 10
p2 = Parrot "Der Schatz ist im Silbersee!" 5

-- Papagei überfahren
runOverParrot :: Parrot -> Parrot
runOverParrot (Parrot _ weight) = Parrot "" weight
-}
-- Ein Tier ist eins der folgenden:
-- - Papagei
-- - Gürteltier
-- LEIDER NEIN:
-- data Animal = Parrot | Dillo
-- MAN MÜSSTE:
-- data Animal = AParrot Parrot | ADillo Dillo

-- algebraischer Datentyp
data Animal weight =
    Dillo { dilloLiveness :: Liveness,
            dilloWeight   :: weight }
  | Parrot String weight
  deriving Show

data Weight = Kg Integer | Pound Integer
  deriving Show

instance Num Weight where
  (Kg a) + (Kg b) = Kg (a + b)

mapAnimal :: (a -> b) -> Animal a -> Animal b
mapAnimal f (Dillo liveness weight) = Dillo liveness (f weight)
mapAnimal f (Parrot sentence weight) = Parrot sentence (f weight)

instance Functor Animal where
    fmap = mapAnimal

-- Tier überfahren
runOverAnimal :: Animal w -> Animal w
runOverAnimal (Dillo _ w) = Dillo Dead w
runOverAnimal (Parrot _ w) = Parrot ""  w

d1' = Parrot "Hallo!" (Kg 5)
feedAnimal amount = fmap (amount +)