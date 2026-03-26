{-# LANGUAGE InstanceSigs #-}
module GameEvent where

import Cards

import qualified Data.Map as Map
import Data.Map (Map)

-- Ein Event ist eine Repräsentation eines Ereignisses.
-- - ist in der Vergangenheit
-- - fachliche Events
-- - enthalten nicht den neuen Zustand
-- Event-Sourcing:
-- - Events erzählen alles
-- - Redundanz OK
{-
data GameEvent =
    DeckShuffled [Card]
  | CardsDealt [Card] [Card] [Card] [Card]
  | PlayersArrived Player Player Player Player
  | CardPlayed Card Player
  | CardsTaken [Card] Player
  | GameEnd Integer Integer Integer Integer
  | PlayerChanged Player

-- vs. Commands: Repräsentation eines Wunsches, daß etwas passieren möge.
-- - in der Zukunft
-- - "noch kein Fakt"
-- - kein Event
data GameCommand =
    NextPlayer Player
  | PlayCard Card Player
-}
data GameEvent
  = HandDealt Player Hand
  | PlayerTurnChanged Player
  | LegalCardPlayed Player Card
  | TrickTaken Player Trick
  | IllegalCardAttempted Player Card
  | GameEnded Player
  deriving (Show, Eq)

data GameCommand
  = DealHands (Map Player Hand)
  | PlayCard Player Card
  deriving (Show, Eq)

-- Spielablauf
data Game a 
    = RecordEvent GameEvent (() -> Game a)
    | IsValid Player Card (Bool -> Game a)
    | RoundOverTrick (Maybe (Trick, Player) -> Game a)
    | PlayerAfter Player (Player -> Game a)
    | GameOver (Maybe Player -> Game a)
    | GetCommand (GameCommand -> Game a)
    | Return a

recordEventM :: GameEvent -> Game ()
recordEventM event = RecordEvent event Return

isValidM :: Player -> Card -> Game Bool
isValidM player card = IsValid player card Return

roundOverTrickM :: Game (Maybe (Trick, Player))
roundOverTrickM = RoundOverTrick Return

playerAfterM :: Player -> Game Player
playerAfterM player = PlayerAfter player Return

gameOverM :: Game (Maybe Player)
gameOverM = GameOver Return

instance Functor Game where

instance Applicative Game where

instance Monad Game where
    return :: a -> Game a
    return = Return
    (>>=) :: Game a -> (a -> Game b) -> Game b
    (>>=) (RecordEvent event callback) next =
        RecordEvent event (\() -> callback () >>= next)
    (>>=) (IsValid player card callback) next =
        IsValid player card (\valid -> callback valid >>= next)
    (>>=) (RoundOverTrick callback) next =
      RoundOverTrick ( \over -> callback over >>= next)
    (>>=) (PlayerAfter player callback) next =
      PlayerAfter player ( \player -> callback player >>= next)
    (>>=) (GameOver callback) next =
      GameOver ( \won -> callback won >>= next)
    (>>=) (GetCommand callback) next =
      GetCommand (\command -> callback command >>= next)
    (>>=) (Return result) next = next result

tableProcessCommandM :: GameCommand -> Game (Maybe Player) -- wenn Spiel vorbei
tableProcessCommandM (DealHands hands) = 
    let pairs = Map.toList hands
        events = fmap (uncurry HandDealt) pairs 
    in do mapM_ recordEventM events
          return Nothing
tableProcessCommandM (PlayCard player card) = 
--    recordEventM (IllegalCardAttempted player card)
--      >>= (\() -> return Nothing)
    do valid <- isValidM player card
       if valid
       then do
        recordEventM (LegalCardPlayed player card)
        roundOverTrick <- roundOverTrickM
        case roundOverTrick of
          Just (trick, trickTaker) ->
            do
              recordEventM (TrickTaken trickTaker trick)
              over <- gameOverM
              case over of
                Just winner ->
                  do
                    recordEventM (GameEnded winner)
                    return (Just winner)
                Nothing ->
                  do
                    recordEventM (PlayerTurnChanged trickTaker)
                    return Nothing
          Nothing ->
            do
              nextPlayer <- playerAfterM player
              recordEventM (PlayerTurnChanged nextPlayer)
              return Nothing
       else do
        recordEventM (IllegalCardAttempted player card)
        return Nothing

-- Gesamtes Spiel spielen
tableLoopM :: GameCommand -> Game Player
tableLoopM command = do
  result <- tableProcessCommandM command
  case result of
    Nothing -> GetCommand tableLoopM
    Just winner -> return winner
