-- Das französische Blatt
module Cards where

-- Datenanalyse für Spielkarten

-- Liste aller Spielkarten des französischen Blatts
-- Denkt an die Listenfunktionen, die Ihr schon kennt!

-- "Make illegable states unrepresentable."

data Suit = Diamonds | Clubs | Spades | Hearts
  deriving (Show, Eq, Ord)

-- |Liste aller Farben
allSuits :: [Suit]
allSuits = [Spades, Hearts, Diamonds, Clubs]

data Rank = Two | Three | Four | Five | Six | Seven | Eight | Nine | Ten
         | Jack | Queen | King | Ace
  deriving (Show, Eq, Ord)

-- |Liste aller Werte
allRanks :: [Rank]
allRanks = [Two, Three, Four, Five, Six, Seven, Eight, Nine, Ten, 
            Jack, Queen, King, Ace]

-- |Spielkarte
data Card = Card { suit :: Suit, rank :: Rank }
  deriving (Show, Eq, Ord)

deck :: [Card]
deck = 
    concatMap (\suit -> map (\rank -> Card suit rank) allRanks) allSuits

deck' =
    let suits = take 52 (cycle allSuits)
        ranks = take 52 (cycle allRanks)
    in map (uncurry Card) (zip suits ranks)


-- Kreuzprodukt: alle Kombinationen aus zweio Listen
cartesianProduct :: [a] -> [b] -> [(a, b)]
cartesianProduct list1 list2 =
  concat (map (\el1 -> map (\el2 -> (el1, el2)) list2) list1)

