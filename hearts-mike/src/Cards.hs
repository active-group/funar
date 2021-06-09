-- Das französische Blatt
module Cards where

import qualified Data.Set as Set
import Data.Set (Set)

import qualified Data.Map.Strict as Map
import Data.Map.Strict (Map, (!))

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

-- |Eine Karte kann nur eine andere Karte gleicher Farbe nach Wert schlagen
cardBeats :: Card -> Card -> Maybe Bool
cardBeats c1 c2 = 
  if suit c1 == suit c2
  then Just (rank c1 > rank c2)
  else Nothing

cartesianProduct :: [a] -> [b] -> [(a, b)]
cartesianProduct list1 list2 =
  concatMap (\ el1 -> map (\ el2 -> (el1, el2)) list2) list1

-- |Das ganze Kartenspiel
deck :: [Card]
deck = map (uncurry Card) (cartesianProduct allSuits allRanks)

-- |Karten, die jemand auf der Hand hält
type Hand = Set Card

-- |Ist die Hand leer?
isHandEmpty :: Hand -> Bool
isHandEmpty hand = Set.null hand

-- |Enthält die Hand eine spezifische Karte?
containsCard :: Card -> Hand -> Bool
containsCard card hand = Set.member card hand

-- |Karte aus der Hand entfernen
removeCard :: Card -> Hand -> Hand
removeCard card hand = Set.delete card hand

-- |Leere Hand
emptyHand :: Hand
emptyHand = Set.empty

-- Spieler

data Player = Player { playerId :: String, playerName :: String }
  deriving Show

instance Eq Player where
  Player id1 _ == Player id2 _ = id1 == id2

instance Ord Player where
  compare (Player id1 _) (Player id2 _) = compare id1 id2

-- * Stich

-- Letzte Karte liegt oben
type Trick = [(Player, Card)]

-- leeren Stich herstellen
emptyTrick :: Trick
emptyTrick = []

-- ist Stich leer
trickEmpty :: Trick -> Bool
trickEmpty trick = null trick

-- wie groß ist der Stich?
trickSize :: Trick -> Int
trickSize trick = length trick

-- alle Karten desStich
cardsOfTrick :: Trick -> [Card]
cardsOfTrick trick = map snd trick

-- Karte auf den Stich legen
addToTrick :: Player -> Card -> Trick -> Trick
addToTrick player card trick = (player, card) : trick

-- die Karte des Stich, die bedient werden muß
leadingCardOfTrick :: Trick -> Card
leadingCardOfTrick trick = snd (last trick)

-- Hände der Spieler
type PlayerHands  = Map Player Hand
