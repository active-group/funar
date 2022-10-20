module Table where

import qualified Data.Foldable as Foldable

import qualified Data.Set as Set
import Data.Set (Set)

import qualified Data.Map.Strict as Map
import Data.Map.Strict (Map, (!))

import Cards
import GameEvent

import Debug.Trace (trace)

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

turnOverTrick :: TableState -> Maybe (Trick, Player)
turnOverTrick state =
  if turnOver state
  then
    let trick = tableStateTrick state
    in Just (trick, whoTakesTrick trick)
  else Nothing

-- Wert eines Stapels
pileScore :: Pile -> Integer
pileScore pile = sum (map cardScore (Set.toList pile))

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

tableProcessEvent :: GameEvent -> TableState -> TableState
tableProcessEvent (HandDealt player hand) state =
  state
    { tableStateHands = Map.insert player hand (tableStateHands state),
      tableStateTrick = emptyTrick
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
        addToPile (tableStatePiles state) player (cardsOfTrick trick),
      tableStateTrick = emptyTrick
    }
tableProcessEvent (IllegalCardAttempted player card) state = state
tableProcessEvent (GameEnded player) state = state

tableProcessCommand :: GameCommand -> TableState -> [GameEvent]
tableProcessCommand (DealHands hands) state =
  -- paar Checks wären noch gut!
  map (uncurry HandDealt) (Map.toList hands)
tableProcessCommand (PlayCard player card) state = 
  if playValid state player card
  then 
    let event1 = LegalCardPlayed player card
        state1 = tableProcessEvent event1 state
    in case turnOverTrick state1 of
         Just (trick, trickTaker) ->
          let event2 = TrickTaken trickTaker trick
              state2 = tableProcessEvent event2 state1
          in 
            case gameOver state2 of
              Nothing -> [event1, event2, PlayerTurnChanged trickTaker]
              Just winner -> 
                [event1, event2, GameEnded winner]
         Nothing ->
          [event1, PlayerTurnChanged (playerAfter state1 player)]
  else
    [IllegalCardAttempted player card]

-- Functor:
-- fmap ::    (a -> b) -> f a -> f b
-- Applicative:
-- (<*>) :: f (a -> b) -> f a -> f b
-- Monad:
-- (>>=) :: m a -> (a -> m b) -> m b

data Game a = -- brauchen Typparameter
  -- pro Operation einen Konstruktor
    PlayValid Player Card (Bool -> Game a)
   | RecordEvent GameEvent (() -> Game a)
 | GetCommand (GameCommand -> Game a)
 | TurnOverTrick (Maybe (Trick, Player) -> Game a)
 | PlayerAfter Player (Player -> Game a)
 | GameOver (Maybe Player -> Game a)
 | Done a

data GameStep a =
   RecordedEvent GameEvent (() -> Game a)
 | NeedsCommand (GameCommand -> Game a)
 | GameDone a

runGameStep :: Game a -> TableState -> (GameStep a, TableState)
runGameStep (PlayValid player card cont) state =
  runGameStep (cont (playValid state player card)) state
runGameStep (TurnOverTrick cont) state =
  runGameStep (cont (turnOverTrick state)) state
runGameStep (PlayerAfter player cont) state =
  runGameStep (cont (playerAfter state player)) state
runGameStep (GameOver cont) state =
  runGameStep (cont (gameOver state)) state
runGameStep (RecordEvent event cont) state =
  (RecordedEvent event cont, tableProcessEvent event state)
runGameStep (GetCommand cont) state =
  (NeedsCommand cont, state)
runGameStep (Done result) state =
  (GameDone result, state)

playValidM :: Player -> Card -> Game Bool
playValidM player card = PlayValid player card Done
recordEventM :: GameEvent -> Game ()
recordEventM event = RecordEvent event Done
turnOverTrickM :: Game (Maybe (Trick, Player))
turnOverTrickM = TurnOverTrick Done
playerAfterM player = PlayerAfter player Done
gameOverM = GameOver Done

instance Functor Game where

instance Applicative Game where

instance Monad Game where
  return = Done
  (PlayValid player card cont) >>= next =
    PlayValid player card (\valid ->
      cont valid >>= next)
  (RecordEvent event cont) >>= next =
    RecordEvent event (\() ->
      cont () >>= next)
  (GetCommand cont) >>= next =
    GetCommand (\command ->
      cont command >>= next)
  (TurnOverTrick cont) >>= next =
    TurnOverTrick (\over ->
      cont over >>= next)
  (PlayerAfter player cont) >>= next =
    PlayerAfter player (\player ->
      cont player >>= next)
  (GameOver cont) >>= next =
    GameOver (\won ->
      cont won >>= next)
  (Done result) >>= next = next result


-- Einen Spielschritt verarbeiten
tableProcessCommandM :: GameCommand -> Game (Maybe Player)
tableProcessCommandM (DealHands hands) =
  do mapM_ (recordEventM . uncurry HandDealt) (Map.toList hands)
     return Nothing
tableProcessCommandM (PlayCard player card) =
  do valid <- playValidM player card
     if valid
     then
      do recordEventM (LegalCardPlayed player card)
         turnOverTrick <- turnOverTrickM
         case turnOverTrick of
          Just (trick, trickTaker) ->
            do recordEventM (TrickTaken trickTaker trick)
               over <- gameOverM
               case over of
                 Just winner -> 
                   do recordEventM (GameEnded winner)
                      return (Just winner)
                 Nothing -> 
                   do recordEventM (PlayerTurnChanged trickTaker)
                      return Nothing
          Nothing -> 
            do nextPlayer <- playerAfterM player
               recordEventM (PlayerTurnChanged nextPlayer)
               return Nothing
     else
      do recordEventM (IllegalCardAttempted player card)
         return Nothing

-- Gesamtes Spiel spielen 
tableLoopM :: GameCommand -> Game (Maybe Player)
tableLoopM command =
  do maybeWinner <- tableProcessCommandM command
     case maybeWinner of
      Nothing -> 
        GetCommand tableLoopM
      Just winner ->
        return maybeWinner

