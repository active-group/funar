module Cards where

-- das französische Blatt

-- Yaron Minski: make illegal states unrepresentable

-- 1 Datenmodell (Typen) für:
--   Farbe (Suit) (Kreuz, Pik, Herz, Karo; Clubs, Spades, Hearts, Diamonds)
--   Wert (Rank) (2 bis Ass) (Jack, Queen, King, Ace)
--   Karte  

data Suit = Clubs | Spades | Hearts | Diamonds
    deriving (Eq, Show, Enum, Bounded, Ord)

data Rank = Two | Three | Four | Five | Six | Seven | Eight | Nine | Ten | Jack | Queen | King | Ace
    deriving (Eq, Show, Enum, Bounded, Ord)

data Card = MkCard Suit Rank
    deriving (Eq, Show, Ord)

allValues :: (Enum a, Bounded a) => [a]
allValues = [minBound .. maxBound]

-- allCards :: [Card]
allCards :: [Card]
allCards = do
    rank <- allValues
    suit <- allValues
    return (MkCard rank suit)

allCards' :: [Card]
allCards' = [ MkCard r s | r <- allValues, s <- allValues ]

cartesianProduct :: [a] -> [b] -> [(a, b)]
cartesianProduct = undefined

-- type Card = (Farbe, Wert)
-- data Card = MkCard

-- 2 Liste aller Karten, ohne alle Karten aufzuzählen

-- 3 Funktion: schlägt eine Karte eine andere?
--   (Karten anderer Farbe sind nicht vergleichbar)

-- cardBeats :: Card -> Card -> Bool