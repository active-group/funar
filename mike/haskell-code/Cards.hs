module Cards where

-- Datenanalyse/Datentypen für Spielkarten des französischen Blattes

-- Yaron Minsky: "Make illegal states unrepresentable."

-- Liste aller Karten, ohne alle aufzulisten
-- (alle Wertigkeiten, Farben auflisten OK)

-- Funktion, die ermittelt, ob eine Karte eine andere (gleicher Farbe) schlägt

-- Alle Karten aufzählen: Alle Kombinationen aus Elementen zweier Listen

cartesianProduct :: [a] -> [b] -> [(a, b)]
-- >>> cartesianProduct [1,2,3] [4,5,6]
-- [(1,4),(1,5),(1,6),(2,4),(2,5),(2,6),(3,4),(3,5),(3,6)]
-- cartesianProduct [] bs = []
-- cartesianProduct (a:as) bs =
--    cartesianProduct1 a bs ++ cartesianProduct as bs
cartesianProduct as bs =
    concat (map (\a -> cartesianProduct1 a bs) as)

cartesianProduct1 :: a -> [b] -> [(a, b)]
-- >>> cartesianProduct1 42 [1,2,3]
-- [(42,1),(42,2),(42,3)]
-- cartesianProduct1 a [] = []
-- cartesianProduct1 a (b:bs) = (a, b) : cartesianProduct1 a bs
cartesianProduct1 a bs = map (\b -> (a, b)) bs 

data Suit = Hearts | Spades
  deriving (Enum, Show)
data Rank = Two | Three | Four
  deriving (Enum, Show)

data Card = MkCard Suit Rank
  deriving Show

cardTuples = cartesianProduct [Hearts .. Spades] [Two .. Four]

deck = map (uncurry MkCard) cardTuples

-- >>> :type MkCard
-- MkCard :: Suit -> Rank -> Card

-- >>> cartesianProduct [Hearts .. Spades] [Two .. Four]
-- [(Hearts,Two),(Hearts,Three),(Hearts,Four),(Spades,Two),(Spades,Three),(Spades,Four)]