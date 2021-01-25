module Cards where

data Suit = Diamonds | Clubs | Spades | Hearts
  deriving (Show, Eq, Ord)

allSuits = [Diamonds, Clubs, Spades, Hearts]

-- Spades, Hearts, Diamonds, Clubs
-- Jack, Queen, King, Ace
data Rank = Two | Three | Four | Five | Six | Seven | Eight | Nine | Ten
         | Jack | Queen | King | Ace
  deriving (Show, Eq, Ord)

allRanks = [Two, Three, Four, Five, Six, Seven, Eight, Nine, Ten, 
            Jack, Queen, King, Ace]

-- "Make invalid states unrepresentable."
data Rank' = Numeric Int | Jack' | Queen' | King' | Ace'
  deriving (Show, Eq, Ord)

-- |Spielkarte
data Card = Card { suit :: Suit, rank :: Rank }
  deriving (Show, Eq, Ord)

allCards :: [Suit] -> [Rank] -> [Card]
allCards suits ranks = 
--  concat (map (\ suit -> map (\ rank -> Card suit rank) ranks) suits)
  concat (map (\ suit -> map (Card suit) ranks) suits)

-- kartesisches Produkt
cartesianProduct :: [a] -> [b] -> [(a, b)]
cartesianProduct as bs = 
  concat (map (\ a -> map (\ b -> (a, b)) bs) as)



-- alle Spielkarten
deck :: [Card]
deck = map (uncurry Card) (cartesianProduct allSuits allRanks)

