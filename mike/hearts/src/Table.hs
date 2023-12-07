module Table(TableState, emptyTableState,
             runTable)
where

import qualified Data.Foldable as Foldable

import qualified Data.Set as Set
import Data.Set (Set)

import qualified Data.Map.Strict as Map
import Data.Map.Strict (Map, (!))

import Cards
import GameEvent

import Debug.Trace (trace)

-- Wert einer Karte
cardScore :: Card -> Integer
cardScore (Card Spades Queen) = 13
cardScore (Card Hearts _) = 1
cardScore _ = 0

-- * Spiellogik

-- Hände der Spieler
type PlayerHands = Map Player Hand

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
    tableStatePiles = Map.fromList (map (\ player -> (player, emptyPile)) players),
    tableStateTrick = emptyTrick
  }

-- ist das Spiel noch am Anfang?
gameAtBeginning :: TableState -> Bool
gameAtBeginning tableState =
  (trickEmpty (tableStateTrick tableState)) && 
    (all pileEmpty (Map.elems (tableStatePiles tableState)))

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
  containsCard card hand &&
  (trickEmpty trick ||
   (let firstCard = leadingCardOfTrick trick
        firstSuit = suit firstCard
    in  suit card == firstSuit -- ok if suit is followed
        || all (\card -> suit card /= firstSuit) (handCards hand))) && -- ok if no such suit in hand
  if gameAtBeginning tableState
  then card ==  Card Clubs Two
  else currentPlayer tableState == player

-- ist diese Runde vorbei?
turnOver :: TableState -> Bool
turnOver state =
  length (tableStatePlayers state) == length (trickToList (tableStateTrick state))

-- | Eine Karte kann nur eine andere Karte gleicher Farbe nach Wert schlagen
replaceIfHigher :: (a, Card) -> (a, Card) -> (a, Card)
replaceIfHigher p1@(marker1, c1) p2@(marker2, c2) =
  case cardOrder c1 c2 of
    Just LT -> p2
    _ -> p1

-- wer muß den Stich einziehen?
whoTakesTrick :: Trick -> Maybe Player
whoTakesTrick (Trick []) = Nothing
whoTakesTrick (Trick list) =
  case reverse list of 
    [] -> Nothing
    (p0 : rest') ->
      let (player, _) = foldl replaceIfHigher p0 rest'
      in Just player

turnOverTrick :: TableState -> Maybe (Player, Trick)
turnOverTrick state =
  if turnOver state
    then
      let trick = tableStateTrick state
       in fmap (\player -> (player, trick)) (whoTakesTrick trick)
    else Nothing

-- Wert eines Stapels
pileScore :: Pile -> Integer
pileScore pile = sum (map cardScore (pileCards pile))

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
addTrickToPile :: PlayerPiles -> Player -> Trick -> PlayerPiles
addTrickToPile playerPiles player trick =
  let playerPile = Map.findWithDefault emptyPile player playerPiles
  in Map.insert player (pileAddTrick playerPile trick) playerPiles

dealHand :: Player -> Hand -> PlayerHands -> PlayerHands
dealHand player hand hands = Map.insert player hand hands

-- Ereignis in den Zustand einarbeiten
tableProcessEvent :: GameEvent -> TableState -> TableState
-- processGameEvent event state | trace ("processGameEvent " ++ show state ++ " " ++ show event) False = undefined
tableProcessEvent (HandDealt player hand) state =
  state
    { tableStateHands = dealHand player hand (tableStateHands state)
    }
tableProcessEvent (PlayerTurnChanged player) state =
  state
    { tableStatePlayers = rotateTo player (tableStatePlayers state)
    }
tableProcessEvent (LegalCardPlayed player card) state =
  state
    { tableStateHands = playCard (tableStateHands state) player card,
      tableStateTrick = addToTrick player card (tableStateTrick state)
    }
tableProcessEvent (TrickTaken player trick) state =
  state
    { tableStatePiles =
        addTrickToPile (tableStatePiles state) player trick,
      tableStateTrick = emptyTrick
    }
tableProcessEvent (IllegalCardAttempted player card) state = state
tableProcessEvent (GameEnded player) state = state

-- data Either left right = Left left | Right right
-- Konvention: Right ist für "korrekt" / "fertig"
-- Left: "fehlerhaft" / "unfertig"

-- runTable :: Game a -> (TableState, [GameEvent]) -> (a, TableState, [GameEvent])
--                                ^^^^^^^^^^^ umgekehrte Reihenfolge
runTable :: Game a
  -> (TableState, [GameEvent])
  -> (Either (GameCommand -> Game a) a, TableState, [GameEvent])
runTable (PlayValid player card callback) s@(state, _) =
  runTable (callback (playValid state player card)) s
runTable (TurnOverTrick callback) s@(state, _) =
  runTable (callback (turnOverTrick state)) s
runTable (PlayerAfter player callback) s@(state, _) =
  runTable (callback (playerAfter state player)) s
runTable (GameOver callback) s@(state, _) =
  runTable (callback (gameOver state)) s

runTable (RecordEvent event callback) s@(state, revents) =
  runTable (callback ()) (tableProcessEvent event state, event:revents)

runTable (Done result) (state, revents) =
  (Right result, state, reverse revents)

runTable (GetCommand callback) s@(state, revents) =
  (Left callback, state, reverse revents)


