module Cards where

-- Domänenmodell für das französische Blatt
-- bzw. für Karten
-- inkl. Kartenvergleich anhand der Wertigkeit

-- Liste aller Karten

data Suit = Diamonds | Clubs | Spades | Hearts
  deriving (Show, Eq, Ord)

-- | Liste aller Farben
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
  deriving (Show, Eq, Ord) -- in der Reihenfolge


-- | Liste aller Werte
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

{-
rankValue :: Rank -> Integer 
rankValue Two = 2
rankValue Three = 3
-}

-- | Spielkarte
-- "make illegal states unrepresentable"
data Card = Card { suit :: Suit, rank :: Rank }
  deriving Show

-- Schlägt 1. Karte die 2. Karte?
cardBeats :: Card -> Card -> Maybe Bool
cardBeats card1 card2 =
    if suit card1 == suit card2 
    then Just (rank card1 > rank card2)
    else Nothing

-- concat :: [[a]] -> [a]

cartesianProduct :: [a] -> [b] -> [(a, b)]
cartesianProduct list1 list2 =
   concat (map (\ a -> map (\ b -> (a, b)) list2) list1)

deck :: [Card]
deck = map (uncurry Card) (cartesianProduct allSuits allRanks)