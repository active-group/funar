-- Das französische Blatt
module Cards where

import qualified Data.Set as Set
import Data.Set (Set)

import qualified Data.Map.Strict as Map
import Data.Map.Strict (Map, (!))

-- "Make illegal states unrepresentable." - Yaron Minsky

-- Illegale Werte gar nicht herstellen.

-- 1. Typsystem
-- 2. Validierung

data Suit = Diamonds | Clubs | Spades | Hearts
    deriving (Show, Eq, Ord)

-- >>> Diamonds < Clubs
-- True

allSuits :: [Suit]
allSuits = [Diamonds, Clubs, Spades, Hearts]

data Rank =
    Two | Three | Four | Five | Six | Seven | Eight
  | Nine | Ten | Jack | Queen | King | Ace
  deriving (Show, Eq, Ord)

allRanks :: [Rank]
allRanks =
  [ Two
    , Three
    , Four
    , Five
    , Six
    , Seven
    , Eight
    , Nine
    , Ten
    , Jack
    , Queen
    , King
    , Ace
  ]

data Card = Card { suit :: Suit, rank :: Rank }
  deriving (Show, Eq, Ord)

cartesianProduct :: [a] -> [b] -> [(a, b)]
cartesianProduct as bs =
    concat (map (\ a -> map (\b -> (a, b)) bs) as)

deck :: [Card]
deck = map (uncurry Card) (cartesianProduct allSuits allRanks)

-- >>> length deck
-- 52

cardOrder :: Card -> Card -> Maybe Ordering
cardOrder card1 card2 =
    if suit card1 == suit card2
    then Just (compare (rank card1) (rank card2))
    else Nothing

-- | Karten, die jemand auf der Hand hält
newtype Hand = Hand {unHand :: Set Card}
  deriving (Eq, Show)

makeHand :: [Card] -> Hand
makeHand cards = Hand (Set.fromList cards)

handCards :: Hand -> [Card]
handCards (Hand set) = Set.toList set

-- | Ist die Hand leer?
isHandEmpty :: Hand -> Bool
isHandEmpty hand = Set.null (unHand hand)

-- | Enthält die Hand eine spezifische Karte?
containsCard :: Card -> Hand -> Bool
containsCard card hand = Set.member card (unHand hand)

-- | Karte aus der Hand entfernen
removeCard :: Card -> Hand -> Hand
removeCard card hand = Hand (Set.delete card (unHand hand))

-- | Leere Hand
emptyHand :: Hand
emptyHand = Hand Set.empty

-- Spieler

data Player = Player {playerName :: String}
  deriving (Show, Eq, Ord)

-- * Stich

-- | Zuletzt gespielte Karte zuerst
newtype Trick = Trick {trickToList :: [(Player, Card)]}
  deriving (Show, Eq)

listToTrick :: [(Player, Card)] -> Trick
listToTrick = Trick

-- | leeren Stich herstellen
emptyTrick :: Trick
emptyTrick = Trick []

-- | ist Stich leer
trickEmpty :: Trick -> Bool
trickEmpty (Trick list) = null list

-- | alle Karten des Stich
cardsOfTrick :: Trick -> [Card]
cardsOfTrick (Trick list) = map snd list

-- | Karte auf den Stich legen
addToTrick :: Player -> Card -> Trick -> Trick
addToTrick player card (Trick list) = Trick ((player, card) : list)

-- | die Karte des Stich, die bedient werden muß
leadingCardOfTrick :: Trick -> Card
leadingCardOfTrick (Trick list) = snd (last list)

-- | Haufen aufgenommener Karten
newtype Pile = Pile {unPile :: Set Card}
  deriving (Show)

emptyPile :: Pile
emptyPile = Pile Set.empty

pileEmpty :: Pile -> Bool
pileEmpty pile = Set.null (unPile pile)

-- Stich auf den Haufen legen
pileAddTrick :: Pile -> Trick -> Pile
pileAddTrick pile trick =
  Pile (Set.union (unPile pile) (Set.fromList (cardsOfTrick trick)))

pileCards :: Pile -> [Card]
pileCards pile = Set.toList (unPile pile)
