module Cards where

import qualified Data.Set as Set
import Data.Set (Set)

data Suit = Diamonds | Clubs | Spades | Hearts
  deriving (Show, Eq, Ord)

-- |list of all suits
allSuits :: [Suit]
allSuits = [Spades, Hearts, Diamonds, Clubs]

data Rank = Numeric Integer | Jack | Queen | King | Ace
  deriving (Show, Eq, Ord)

-- |rankBeats r1 r2 returns True, if r1 beats r2
rankBeats :: Rank -> Rank -> Bool
rankBeats r1 r2 = r1 > r2

-- |list of all ranks
allRanks :: [Rank]
allRanks = [Numeric i | i <- [2..10]] ++ [Jack, Queen, King, Ace]

-- |playing cards
data Card = Card { suit :: Suit, rank :: Rank }
  deriving (Show, Eq, Ord)

-- |cardBeats c1 c2 returns True, if c1 beats c2: they have the same suit and c1's rank is higher
cardBeats :: Card -> Card -> Bool
cardBeats givenCard c = suit givenCard == suit c
                        && rankBeats (rank givenCard)
                                     (rank c)

-- |full deck of all cards
deck :: [Card]
deck = [Card suit' rank' | rank' <- allRanks, suit' <- allSuits]

-- |during the game, a hand contains at least one card
type Hand = Set Card

-- |does a hand contain zero cards?
isHandEmpty :: Hand -> Bool
isHandEmpty hand = Set.null hand

-- |does a hand contain a specific card?
containsCard :: Card -> Hand -> Bool
containsCard card hand = Set.member card hand

-- |remove a card from a hand
removeCard :: Card -> Hand -> Hand
removeCard card hand = Set.delete card hand

-- |empty hand
emptyHand :: Hand
emptyHand = Set.empty

-- |pretty-print card rank
prettyRank :: Rank -> String
prettyRank (Numeric i) = show i
prettyRank r = show r

-- |pretty-print card suit
prettySuit :: Suit -> String
prettySuit s = show s

-- |pretty-print card
prettyCard :: Card -> String
prettyCard c = prettyRank (rank c) ++ " of " ++ prettySuit (suit c)

-- |pretty-print list of cards
prettyCards :: [Card] -> String
prettyCards [] = ""
prettyCards [x] = prettyCard x
prettyCards (x:xs) = prettyCard x ++ " and\n" ++ prettyCards xs

{-
-- Eine Karte hat folgende Eigenschaften:
-- - Farbe (suit)
-- - Wert (rank)
data Card = Card { suit :: Suit, rank :: Rank }
  deriving (Ord, Eq, Show)

-- Eine Farbe ist eine der folgenden:
-- Pik Karo Herz Kreuz
data Suit = Diamonds | Clubs | Spades | Hearts
  deriving (Ord, Eq, Show)

-- alle Farben
suits :: [Suit]
suits = [Diamonds, Clubs, Spades, Hearts]

-- Ein Wert ist eins der folgenden:
-- 2 .. 10
-- Bube Dame König As
-- bessere Modell:
data Rank = Two | Three | Four | Five | Six | Seven | Eight | Nine | Ten | Jack | Queen | King | Ace
  deriving (Show, Eq, Ord)
-- Int: nicht beschränkt auf 2-10
-- data Rank = Numeric Int | Jack | Queen | King | Ace

-- alle Werte
ranks :: [Rank]
ranks = [Two, Three, Four, Five , Six , Seven , Eight , Nine , Ten , Jack , Queen , King , Ace]

-- Wert höher als ein anderer
rankHigher :: Rank -> Rank -> Bool
rankHigher rank1 rank2 = rank1 > rank2

deck :: Set Card
deck = Set.map (uncurry Card)
               (Set.cartesianProduct (Set.fromList suits) (Set.fromList ranks))
-}
