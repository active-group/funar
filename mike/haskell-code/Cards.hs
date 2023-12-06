module Cards where

-- Datenmodell für Karten des französischen Blattes

-- Yaron Minsky: "Make illegal states unrepresentable."

-- Liste aller Karten des französischen Blatts
-- ... aus jeweils den Listen der Farben und Wertigkeiten
-- ... dafür gibt's Library-Funktionen
-- (Allgemeinverwendbare Abstraktion?)

-- Funktion, die feststellt, ob eine andere Karte
-- höherwertig ist - aber nur bei gleicher Farbe.

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
  deriving (Show, Eq, Ord)

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

-- | Spielkarte
data Card = Card {suit :: Suit, rank :: Rank}
  deriving (Show, Eq, Ord)

-- | Eine Karte kann nur eine andere Karte gleicher Farbe nach Wert schlagen
cardOrder :: Card -> Card -> Maybe Ordering
cardOrder c1 c2 =
  if suit c1 == suit c2
    then Just (compare (rank c1) (rank c2))
    else Nothing

cartesianProduct :: [a] -> [b] -> [(a, b)]
cartesianProduct list1 list2 =
  concat (map (\el1 -> map (\el2 -> (el1, el2)) list2) list1)

cartesianProduct' :: (a -> b -> c) -> [a] -> [b] -> [c]
cartesianProduct' c list1 list2 =
  concat (map (\el1 -> map (\el2 -> c el1 el2) list2) list1)

cartesianProduct'' :: [a] -> [b] -> [(a, b)]
cartesianProduct'' list1 list2 =
    map (,) list1 <*> list2

-- deck = map (uncurry Card) (cartesianProduct allSuits allRanks)
-- deck = cartesianProduct' Card allSuits allRanks
deck :: [Card]
-- >>> length deck
-- 52
deck = map Card allSuits <*> allRanks
