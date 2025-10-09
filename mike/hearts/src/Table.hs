module Table(TableState, emptyTableState,
             runTable)
where

import qualified Data.Foldable as Foldable

import qualified Data.Set as Set
import Data.Set (Set)

import qualified Data.Map.Strict as Map
import Data.Map.Strict (Map, (!))

import qualified Data.Stream as Stream
import Data.Stream (Stream)

import Cards
import GameEvent

import Debug.Trace (trace)

-- | Wert einer Karte
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
  { tableStatePlayers :: [Player],
    tableStateNext :: Stream Player, -- unendliche Liste
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
    tableStateNext = Stream.cycle players,
    tableStateHands = Map.fromList (map (\ player -> (player, emptyHand)) players),
    tableStatePiles = Map.fromList (map (\ player -> (player, emptyPile)) players),
    tableStateTrick = emptyTrick
  }

-- $setup
-- >>> let mike = Player "Mike"
-- >>> let nicole = Player "Nicole"
-- >>> let peter = Player "Peter"
-- >>> let annette = Player "Annette"
-- >>> let players = [mike, nicole, peter, annette]
-- >>> let state0 = emptyTableState players

-- | ist das Spiel noch am Anfang?
-- >>> gameAtBeginning state0
-- True
gameAtBeginning :: TableState -> Bool
gameAtBeginning tableState =
  (trickEmpty (tableStateTrick tableState)) && 
    (all pileEmpty (Map.elems (tableStatePiles tableState)))

-- | wer ist als nächstes dran?
-- >>> playerAfter state0 mike
-- Player {playerName = "Nicole"}
playerAfter :: TableState -> Player -> Player
playerAfter state player =
  Stream.head (Stream.tail (skipTo player (tableStateNext state)))

-- | Liste zu einem bestimmten Element rotieren
-- >>> Stream.take 3 (skipTo 3 (Stream.cycle [1,2,3,4,5]))
-- [3,4,5]
skipTo :: Eq a => a -> Stream a -> Stream a
skipTo x stream = Stream.dropWhile (/= x) stream

-- | wer ist gerade dran?
-- >>> currentPlayer state0
-- Player {playerName = "Mike"}
currentPlayer :: TableState -> Player
currentPlayer state =
  Stream.head (tableStateNext state)

-- | ist es zulässig, diese Karte auszuspielen?
-- >>> playValid state0 mike (Card Clubs Two)
-- False
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

-- | ist diese Runde vorbei?
-- >>> roundOver state0
-- False
roundOver :: TableState -> Bool
roundOver state =
  length (tableStatePlayers state) == length (trickToList (tableStateTrick state))

-- | Eine Karte kann nur eine andere Karte gleicher Farbe nach Wert schlagen
-- >>> replaceIfHigher (1, Card Clubs Two) (2, Card Clubs Three)
-- (2,...
-- >>> replaceIfHigher (1, Card Clubs Three) (2, Card Clubs Two)
-- (1,...
-- >>> replaceIfHigher (1, Card Clubs Two) (2, Card Diamonds Three)
-- (1,...
replaceIfHigher :: (a, Card) -> (a, Card) -> (a, Card)
replaceIfHigher p1@(marker1, c1) p2@(marker2, c2) =
  case cardOrder c1 c2 of
    Just LT -> p2
    _ -> p1

-- | wer muß den Stich einziehen?
-- >>> whoTakesTrick emptyTrick
-- Nothing
-- >>> let trick1 = addToTrick mike (Card Clubs Two) emptyTrick
-- >>> let trick2 = addToTrick nicole (Card Clubs Ten) trick1
-- >>> let trick3 = addToTrick peter (Card Diamonds Two) trick2
-- >>> let trick4 = addToTrick annette (Card Clubs Five) trick3
-- >>> whoTakesTrick trick4
-- Just (Player {playerName = "Nicole"})

whoTakesTrick :: Trick -> Maybe Player
whoTakesTrick trick =
  case reverse (trickToList trick) of 
    [] -> Nothing
    (p0 : rest') ->
      let (player, _) = foldl replaceIfHigher p0 rest'
      in Just player

-- | Falls Runde vorbei, Stich und Aufnehmer:in liefern
roundOverTrick :: TableState -> Maybe (Trick, Player)
roundOverTrick state =
  if roundOver state
  then
    let trick = tableStateTrick state
    in fmap (\player -> (trick, player)) (whoTakesTrick trick)
  else Nothing

-- | Wert eines Stapels
-- >>> let trick1 = addToTrick mike (Card Clubs Two) emptyTrick
-- >>> let trick2 = addToTrick nicole (Card Clubs Ten) trick1
-- >>> let trick3 = addToTrick peter (Card Hearts Two) trick2
-- >>> let trick4 = addToTrick annette (Card Spades Queen) trick3
-- >>> pileScore (pileAddTrick emptyPile trick4)
-- 14
pileScore :: Pile -> Integer
pileScore pile = sum (map cardScore (pileCards pile))

-- | Ist das Spiel vorbei und wenn ja wer hat gewonnen?
gameOver :: TableState -> Maybe Player
gameOver state =
  if all isHandEmpty (Map.elems (tableStateHands state))
  then 
    let playerScores = Map.toList (fmap pileScore (tableStatePiles state))
        cmp (_, score1) (_, score2) = compare score1 score2
    in Just (fst (Foldable.minimumBy cmp playerScores))
  else 
    Nothing

-- | Karte ausspielen
playCard :: PlayerHands -> Player -> Card -> PlayerHands
playCard playerHands player card =
  Map.alter (fmap (removeCard card)) player playerHands

-- | Karten zum Stapel hinzufügen
addTrickToPile :: PlayerPiles -> Player -> Trick -> PlayerPiles
addTrickToPile playerPiles player trick =
  let playerPile = Map.findWithDefault emptyPile player playerPiles
  in Map.insert player (pileAddTrick playerPile trick) playerPiles

dealHand :: Player -> Hand -> PlayerHands -> PlayerHands
dealHand player hand hands = Map.insert player hand hands

-- | Ereignis in den Zustand einarbeiten
tableProcessEvent :: GameEvent -> TableState -> TableState
-- tableProcessEvent event state | trace ("tableProfessEvent " ++ show state ++ " " ++ show event) False = undefined
tableProcessEvent (HandDealt player hand) state =
  state
    { tableStateHands = dealHand player hand (tableStateHands state)
    }
tableProcessEvent (PlayerTurnChanged player) state =
  state
    { tableStateNext = skipTo player (tableStateNext state)
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

runTable :: Game a -> (TableState, [GameEvent]) 
   -> (Either (GameCommand -> Game a) a, TableState, [GameEvent])
--                                 ^^^^^^^^^^^ Akkumulator für Events (umgekehrte Reihenfolge)
--                                                                 ^^^^^^^^^^ alle passierten Events, richtige Reihenfolge
runTable (IsPlayValid player card cont) (state, revents) =
  runTable (cont (playValid state player card)) (state, revents)
runTable (RoundOverTrick cont) (state, revents) =
  runTable (cont (roundOverTrick state)) (state, revents)
runTable (PlayerAfter player cont) (state, revents) =
  runTable (cont (playerAfter state player)) (state, revents)
runTable (GameOver cont) (state, revents) =
  runTable (cont (gameOver state)) (state, revents)

runTable (RecordEvent event cont) (state, revents) =
  let newState = tableProcessEvent event state
  in runTable (cont ()) (newState, event : revents)

runTable (Return result) (state, revents) =
  (Right result, state, reverse revents)

-- data Either l r = Left l | Right r
runTable (GetCommand cont) (state, revents) =
  (Left cont, state, reverse revents)