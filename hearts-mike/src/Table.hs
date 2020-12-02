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

-- wie data für diesen Spezialfall: 1 Konstruktor, 1 Attribut
-- keinen Repräsentationsoverhead 
newtype Stack = Stack (Set Card)
  deriving Show

emptyStack :: Stack
emptyStack = Stack Set.empty


-- eingezogene Karten, pro Spieler
type PlayerStacks = Map Player Stack

data TableState =
  TableState
  { tableStatePlayers :: [Player], -- wer dran ist, steht vorn
    tableStateHands   :: PlayerHands,
    tableStateStacks  :: PlayerStacks,
    tableStateTrick   :: Trick
  }
  deriving Show

-- Anfangszustand herstellen
emptyGameState :: [Player] -> TableState
emptyGameState players =
  TableState {
    tableStatePlayers = players,
    tableStateHands = Map.fromList (map (\ player -> (player, emptyHand)) players),
    tableStateStacks = Map.fromList (map (\ player -> (player, emptyStack)) players),
    tableStateTrick = emptyTrick
  }

-- ist das Spiel noch am Anfang?
gameAtBeginning :: TableState -> Bool
gameAtBeginning gameState =
  (trickEmpty (tableStateTrick gameState)) && 
    (all null (Map.elems (tableStateStacks gameState)))

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
playValid gameState player card =
  let hand = tableStateHands gameState ! player
      trick = tableStateTrick gameState
  in
  legalCard card hand trick &&
  if gameAtBeginning gameState
  then card ==  Card Clubs Two
  else currentPlayer gameState == player

-- ist diese Runde vorbei?
turnOver :: TableState -> Bool
turnOver state =
  length (tableStatePlayers state) == trickSize (tableStateTrick state)

-- Wert eines Stapels
stackScore :: Stack -> Integer
stackScore hand = sum (map cardScore (Set.toList hand))

-- Ist das Spiel vorbei und wenn ja wer hat gewonnen?
gameOver :: TableState -> Maybe Player
gameOver state =
  if all isHandEmpty (Map.elems (tableStateHands state))
  then 
    let playerScores = Map.toList (fmap stackScore (tableStateStacks state))
        cmp (_, score1) (_, score2) = compare score1 score2
    in Just (fst (Foldable.minimumBy cmp playerScores))
  else 
    Nothing

-- Karte ausspielen
takeCard :: PlayerHands -> Player -> Card -> PlayerHands
takeCard playerHands player card =
  Map.alter (fmap (removeCard card)) player playerHands

-- Karten zum Stapel hinzufügen
addToStack :: PlayerStacks -> Player -> [Card] -> PlayerStacks
addToStack playerStacks player cards =
  let playerStack = Map.findWithDefault Set.empty player playerStacks
  in Map.insert player (Set.union playerStack (Set.fromList cards)) playerStacks

-- Verbindung zwischen TableState und GameEvent / GameCommand
tableProcessEvent :: GameEvent -> TableState -> TableState
tableProcessEvent (HandDealt player hand) state =
  state {
    tableStateHands = Map.insert player hand (tableStateHands state),
    tableStateTrick = emptyTrick
  }
tableProcessEvent (PlayerTurnChanged player) state =
  state {
    tableStatePlayers  = rotateTo player (tableStatePlayers state)
  }
tableProcessEvent (LegalCardPlayed player card) state =
  state {
    tableStateHands = takeCard (tableStateHands state) player card,
    tableStateTrick = addToTrick player card (tableStateTrick state)
  }
tableProcessEvent (TrickTaken player trick) state =
  state {
    tableStateStacks =
      addToStack (tableStateStacks state) player (cardsOfTrick trick),
    tableStateTrick = emptyTrick
  }
tableProcessEvent (IllegalCardPlayed player card) state = state
tableProcessEvent (GameEnded player) state = state
