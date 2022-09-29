module Table where

import qualified Data.Foldable as Foldable

import qualified Data.Set as Set
import Data.Set (Set)

import qualified Data.Map.Strict as Map
import Data.Map.Strict (Map, (!))

import Cards
import GameEvent

import Debug.Trace (trace)

-- wer muß den Stich einziehen?
whoTakesTrick :: Trick -> Player
whoTakesTrick [] = undefined
whoTakesTrick trick =
  let loop player _ [] = player
      loop player card ((player', card') : rest) =
        case cardBeats card' card of
          Nothing -> loop player card rest
          Just False -> loop player card rest
          Just True -> loop player' card' rest
      (player0, card0) : rest' = reverse trick
  in loop player0 card0 rest'

-- ist die Karte aus der Hand für den Stich zulässig ?
legalCard :: Card -> Hand -> Trick -> Bool
legalCard card hand trick =
  containsCard card hand &&
  (trickEmpty trick ||
   (let firstCard = leadingCardOfTrick trick
        firstSuit = suit firstCard
    in  suit card == firstSuit -- ok if suit is followed
        || all ((/= firstSuit) . suit) hand)) -- ok if no such suit in hand

-- Wert einer Karte
cardScore :: Card -> Integer
cardScore (Card Spades Queen) = 13
cardScore (Card Hearts _) = 1
cardScore _ = 0

-- * Spiellogik

type Pile = Set Card

-- eingezogene Karten, pro Spieler
type PlayerPiles = Map Player Pile

data TableState =
  TableState
  { tableStatePlayers :: [Player], -- wer dran ist, steht vorn
    tableStateHands   :: PlayerHands,
    tableStatePiles  :: PlayerPiles,
    tableStateTrick   :: Trick
  }
  deriving Show

-- Anfangszustand herstellen
emptyTableState :: [Player] -> TableState
emptyTableState players =
  TableState {
    tableStatePlayers = players,
    tableStateHands = Map.fromList (map (\ player -> (player, emptyHand)) players),
    tableStatePiles = Map.fromList (map (\ player -> (player, Set.empty)) players),
    tableStateTrick = emptyTrick
  }

-- ist das Spiel noch am Anfang?
gameAtBeginning :: TableState -> Bool
gameAtBeginning tableState =
  (trickEmpty (tableStateTrick tableState)) && 
    (all null (Map.elems (tableStatePiles tableState)))

-- wer ist als nächstes dran?
playerAfter :: TableState -> Player -> Player
playerAfter state player =
   head (rotate (rotateTo player (tableStatePlayers state)))

-- Liste rotieren
rotate :: [a] -> [a]
rotate (x : xs) = xs ++ [x]
rotate [] = undefined

-- Liste zu einem bestimmten Element rotieren
rotateTo :: Eq a => a -> [a] -> [a]
rotateTo y xs@(x : xs') | x == y = xs
                        | otherwise = rotateTo y (xs' ++ [x])
rotateTo y [] = undefined


-- wer ist gerade dran?
currentPlayer :: TableState -> Player
currentPlayer state =
  head (tableStatePlayers state)


-- ist es zulässig, diese Karte auszuspielen?
playValid :: TableState -> Player -> Card -> Bool
playValid tableState player card =
  let hand = tableStateHands tableState ! player
      trick = tableStateTrick tableState
  in
  legalCard card hand trick &&
  if gameAtBeginning tableState
  then card ==  Card Clubs Two
  else currentPlayer tableState == player

-- ist diese Runde vorbei?
turnOver :: TableState -> Bool
turnOver state =
  length (tableStatePlayers state) == trickSize (tableStateTrick state)

-- Wert eines Stapels
pileScore :: Pile -> Integer
pileScore hand = sum (map cardScore (Set.toList hand))

-- Ist das Spiel vorbei und wenn ja wer hat gewonnen?
gameOver :: TableState -> Maybe Player
gameOver state =
  if all isHandEmpty (Map.elems (tableStateHands state))
  then 
    let playerScores = Map.toList (fmap pileScore (tableStatePiles state))
        cmp (_, score1) (_, score2) = compare score1 score2
    in Just (fst (Foldable.minimumBy cmp playerScores))
  else 
    Nothing

-- Karte ausspielen
playCard :: PlayerHands -> Player -> Card -> PlayerHands
playCard playerHands player card =
  Map.alter (fmap (removeCard card)) player playerHands

-- Karten zum Stapel hinzufügen
addToPile :: PlayerPiles -> Player -> [Card] -> PlayerPiles
addToPile playerPiles player cards =
  let playerPile = Map.findWithDefault Set.empty player playerPiles
  in Map.insert player (Set.union playerPile (Set.fromList cards)) playerPiles
