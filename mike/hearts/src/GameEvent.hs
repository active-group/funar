{-# LANGUAGE InstanceSigs #-}
module GameEvent where

import Cards

import qualified Data.Map as Map
import Data.Map (Map)

-- Event:
-- - Beschreibung von Ereignis, das passiert ist
-- - Reihenfolge (wahrscheinlich) wichtig
-- - fachlich
-- - (unverändlich)
-- - "vollständig"
-- - Redundanz OK

{-
data GameEvent =
    GameStarted [Player]
  | CardsDealt [(Player, Hand)]
  | PlayerPlayedCard Player Card
  | RoundPlayed Player [Card]
  | GameEnded Player
  | PlayerTurnChanged Player
  deriving Show

-- Command:
-- Beschreibung eines Wunsches, daß etwas passieren möge
-- ... noch nicht in der Vergangenheit
data GameCommand =
    ChooseCard Player Card

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

{-
data DealHandsCommand = DealHands (Map Player Hand)

data PlayCardCommand = PlayCard Player Card

executeDealHandsCommand (DealHands map) = ...

executeDealHandsCommand (DealHands map)
-}

data Game a =
    RecordEvent GameEvent (() -> Game a)
  | IsValidCard Player Card (Bool -> Game a)
  | TurnOverTrick (Maybe (Trick, Player) -> Game a)
  | PlayerAfter Player (Player -> Game a)
  | GameOver (Maybe Player -> Game a)
  | GetCommand (GameCommand -> Game a)
  | Return a

recordEventM :: GameEvent -> Game ()
recordEventM event = RecordEvent event Return

isValidCardM :: Player -> Card -> Game Bool
isValidCardM player card = IsValidCard player card Return

turnOverTrickM :: Game (Maybe (Trick, Player))
turnOverTrickM = TurnOverTrick Return

playerAfterM :: Player -> Game Player
playerAfterM player = PlayerAfter player Return

gameOverM :: Game (Maybe Player)
gameOverM = GameOver Return

instance Functor Game where

instance Applicative Game where
    pure :: a -> Game a
    pure = Return

instance Monad Game where
    (>>=) :: Game a -> (a -> Game b) -> Game b
    (>>=) (RecordEvent event callback) next =
        RecordEvent event (\() -> callback () >>= next)
    (>>=) (IsValidCard player card callback) next =
        IsValidCard player card
          (\valid -> callback valid >>= next)
    (>>=) (TurnOverTrick callback) next =
        TurnOverTrick (\m -> callback m >>= next)
    (>>=) (PlayerAfter player callback) next =
        PlayerAfter player (\player -> callback player >>= next)
    (>>=) (GameOver callback) next =
        GameOver (\m -> callback m >>= next)
    (>>=) (GetCommand callback) next =
        GetCommand (\command -> callback command >>= next)
    (>>=) (Return result) next = next result

-- *ein* Command, Ergebnis: Gewinner, falls Spiel zu Ende
tableProcessCommandM :: GameCommand -> Game (Maybe Player)
tableProcessCommandM (DealHands hand) =
    -- HandDealt-Events verzeichnen
    do let pairs = Map.toList hand
       let events = map (uncurry HandDealt) pairs
       -- M für Monade, _ für "kein Ergebnis"
       mapM_ recordEventM events
       return Nothing -- Spiel noch nicht zu Ende
tableProcessCommandM (PlayCard player card) =
    do valid <- isValidCardM player card
       if valid
       then do recordEventM (LegalCardPlayed player card)
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

       else do recordEventM (IllegalCardAttempted player card)
               return Nothing

tableLoopM :: GameCommand -> Game Player
tableLoopM command =
    do maybeWinner <- tableProcessCommandM command
       case maybeWinner of
        Just winner -> return winner
        Nothing ->
            GetCommand tableLoopM