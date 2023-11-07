module Cards where

-- Datenanalyse für das französische Blatt:
-- => 1. Datentyp für Karten
-- Yaron Minsky: "Make illegal states unrepresentable."

-- 2. Liste aller Karten (Länge: 52)
--    aus Liste aller Farben / Wertigkeiten

-- 3. Funktion, die bei 2 Karten gleicher Farbe testet,
--    welche höherwertig ist

getGreaterCard :: Card -> Card -> Maybe Card
getGreaterCard card1@(MkCard s1 r1) card2@(MkCard s2 r2) =
  if s1 == s2
    then
      if r1 > r2
        then Just card1
        else Just card2
    else Nothing

queenOfHearts = MkCard Hearts Queen

twoOfHearts = MkCard Hearts Two

kingOfSpades = MkCard Spades King

-- >>> getGreaterCard kingOfSpades queenOfHearts
-- Nothing

data Suit
  = Hearts
  | Diamonds
  | Spades
  | Clubs
  deriving (Eq, Show)

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
  deriving (Ord, Eq, Show)

data Card = MkCard {suit :: Suit, rank :: Rank}
  deriving (Show)

-- >>> Ace > Three
-- True
suits = [Hearts, Diamonds, Spades, Clubs]

ranks = [Two, Three, Four, Five, Six, Seven, Eight, Nine, Ten, Jack, Queen, King, Ace]

deck = concatMap (\s -> map (MkCard s) ranks) suits

-- >>> deck
-- [MkCard {suit = Hearts, rank = Two},MkCard {suit = Hearts, rank = Three},MkCard {suit = Hearts, rank = Four},MkCard {suit = Hearts, rank = Five},MkCard {suit = Hearts, rank = Six},MkCard {suit = Hearts, rank = Seven},MkCard {suit = Hearts, rank = Eight},MkCard {suit = Hearts, rank = Nine},MkCard {suit = Hearts, rank = Ten},MkCard {suit = Hearts, rank = Jack},MkCard {suit = Hearts, rank = Queen},MkCard {suit = Hearts, rank = King},MkCard {suit = Hearts, rank = Ace},MkCard {suit = Diamonds, rank = Two},MkCard {suit = Diamonds, rank = Three},MkCard {suit = Diamonds, rank = Four},MkCard {suit = Diamonds, rank = Five},MkCard {suit = Diamonds, rank = Six},MkCard {suit = Diamonds, rank = Seven},MkCard {suit = Diamonds, rank = Eight},MkCard {suit = Diamonds, rank = Nine},MkCard {suit = Diamonds, rank = Ten},MkCard {suit = Diamonds, rank = Jack},MkCard {suit = Diamonds, rank = Queen},MkCard {suit = Diamonds, rank = King},MkCard {suit = Diamonds, rank = Ace},MkCard {suit = Spades, rank = Two},MkCard {suit = Spades, rank = Three},MkCard {suit = Spades, rank = Four},MkCard {suit = Spades, rank = Five},MkCard {suit = Spades, rank = Six},MkCard {suit = Spades, rank = Seven},MkCard {suit = Spades, rank = Eight},MkCard {suit = Spades, rank = Nine},MkCard {suit = Spades, rank = Ten},MkCard {suit = Spades, rank = Jack},MkCard {suit = Spades, rank = Queen},MkCard {suit = Spades, rank = King},MkCard {suit = Spades, rank = Ace},MkCard {suit = Clubs, rank = Two},MkCard {suit = Clubs, rank = Three},MkCard {suit = Clubs, rank = Four},MkCard {suit = Clubs, rank = Five},MkCard {suit = Clubs, rank = Six},MkCard {suit = Clubs, rank = Seven},MkCard {suit = Clubs, rank = Eight},MkCard {suit = Clubs, rank = Nine},MkCard {suit = Clubs, rank = Ten},MkCard {suit = Clubs, rank = Jack},MkCard {suit = Clubs, rank = Queen},MkCard {suit = Clubs, rank = King},MkCard {suit = Clubs, rank = Ace}]
