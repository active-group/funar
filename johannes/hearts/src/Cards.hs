-- Das französische Blatt
module Cards(Suit(..), Rank(..), Card(..),
             deck, cardBeats,
             Player(..), 
             Hand, makeHand, emptyHand, isHandEmpty, containsCard, removeCard, handCards,
             Trick(..), emptyTrick,  trickEmpty, addToTrick, cardsOfTrick, leadingCardOfTrick,
             Pile, emptyPile, pileEmpty, pileAddTrick, pileCards,
             allSuits, allRanks)
where

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
  concat (map (\ el1 -> map (\ el2 -> (el1, el2)) list2) list1)

-- |Das ganze Kartenspiel
deck :: [Card]
deck = map (uncurry Card) (cartesianProduct allSuits allRanks)

-- |Karten, die jemand auf der Hand hält
newtype Hand = Hand { unHand :: Set Card }
  -- unHand (Hand undefined) == undefined
  deriving (Eq, Show)

makeHand :: [Card] -> Hand
makeHand cards = Hand (Set.fromList cards)

handCards :: Hand -> [Card]
handCards (Hand set) = Set.toList set

-- |Ist die Hand leer?
isHandEmpty :: Hand -> Bool
isHandEmpty hand = Set.null (unHand hand)

-- |Enthält die Hand eine spezifische Karte?
containsCard :: Card -> Hand -> Bool
containsCard card hand = Set.member card (unHand hand)

-- |Karte aus der Hand entfernen
removeCard :: Card -> Hand -> Hand
removeCard card hand = Hand (Set.delete card (unHand hand))

-- |Leere Hand
emptyHand :: Hand
emptyHand = Hand Set.empty

-- Spieler

newtype Player = Player { playerName :: String }
  deriving (Show, Eq, Ord)

-- * Stich

-- Zuletzt gespielte Karte zuerst
newtype Trick = Trick { trickToList :: [(Player, Card)] }
  deriving (Show, Eq)

-- leeren Stich herstellen
emptyTrick :: Trick
emptyTrick = Trick []

--- ist Stich leer
trickEmpty :: Trick -> Bool
trickEmpty (Trick list) = null list

-- alle Karten desStich
cardsOfTrick :: Trick -> [Card]
cardsOfTrick (Trick list) = map snd list

-- Karte auf den Stich legen
addToTrick :: Player -> Card -> Trick -> Trick
addToTrick player card (Trick list) = Trick ((player, card) : list)

-- die Karte des Stich, die bedient werden muß
leadingCardOfTrick :: Trick -> Card
leadingCardOfTrick (Trick list) = snd (last list)

-- Haufen aufgenommener Karten
newtype Pile = Pile { unPile :: Set Card }
  deriving Show

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

