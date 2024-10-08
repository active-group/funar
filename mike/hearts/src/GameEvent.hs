module GameEvent where

import Cards

import qualified Data.Map as Map
import Data.Map (Map)

-- Event: Datenrepräsentation von einem Ereignis

-- - ist passiert, in der Vergangenheit
-- - fachlich
-- - vollständig: gesamte Geschichte der Domäne
-- - Redundanz OK
-- - *nicht* Zustand mit ablegen
{-
data GameEvent =
    CardsDealt (Map Player Hand)
  | GameStarted Player Player Player Player
  | CardLaidAtTrickStart Player Card
  | CardLaidIntoTrick Player Card
  | CardsTaken Player [Card]
  | GameWon Player
  deriving Show

-- Command: Repräsentation eines Wunschs, daß etwas passieren soll - in der Zukunft
-- != Event
data GameCommand =
    DealCards Player [Card]
  | StartGame
  | PlayCard Player Card
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
playCard :: Player -> Card -> ... -> ...

--> 

data PlayCardCommand = MkPlayCardCommand Player Card

playCard :: PlayCardCommand -> ... -> ...

playCard p c ... -> playCard (MkPlayerCommand p c) ...

-}

-- ----------------------------------------------------------------------------
-- Abhängigkeiten ^^^^^^^^^^

-- als nächstes: Ablauf des Spiels / Anordnung der Events
-- sequentielle Abläufe: Monade

-- Wie werden aus Commands Events?

data Game a =
    RecordEvent GameEvent (() -> Game a)
  | GetCommand (GameCommand -> Game a)
  | IsPlayCardAllowed Player Card (Bool -> Game a)
  | TurnOverTrick (Maybe (Trick, Player) -> Game a)
  | PlayerAfter Player (Player -> Game a)
  | GameOver (Maybe Player -> Game a)
  | Return a

recordEventM :: GameEvent -> Game ()
recordEventM event = RecordEvent event Return

isPlayCardAllowedM :: Player -> Card -> Game Bool
isPlayCardAllowedM player card = IsPlayCardAllowed player card Return

turnOverTrickM :: Game (Maybe (Trick, Player))
turnOverTrickM = TurnOverTrick Return

playerAfterM :: Player -> Game Player
playerAfterM player = PlayerAfter player Return

gameOverM :: Game (Maybe Player)
gameOverM = GameOver Return

instance Functor Game

instance Applicative Game

instance Monad Game where
  (>>=) (RecordEvent event callback) next =
    RecordEvent event (\() -> callback () >>= next) -- (\x -> callback x >>= next)
  (>>=) (IsPlayCardAllowed player card callback) next =
    IsPlayCardAllowed player card (\allowed -> 
                                    callback allowed >>= next)
  (>>=) (TurnOverTrick cont) next =
   TurnOverTrick
    ( \over ->
        cont over >>= next
    )
  (>>=) (PlayerAfter player cont) next =
   PlayerAfter
    player
    ( \player ->
        cont player >>= next
    )
  (>>=) (GameOver cont) next =
   GameOver
    ( \won ->
        cont won >>= next
    )
  (>>=) (GetCommand cont) next =
    GetCommand (\command -> cont command >>= next)
  (>>=) (Return result) next = next result
  return = Return

-- data Maybe a = Nothing | Just a

-- liefert Gewinner:in
tableProcessCommandM :: GameCommand -> Game (Maybe Player) -- sagt uns, ob das Spiel vorbei ist und wer gewonnen hat
tableProcessCommandM (DealHands hands) =
  let list = Map.toList hands
      events = map (uncurry HandDealt) list
      games = map recordEventM events
  in do sequence_ games
        return Nothing -- Spiel geht weiter

tableProcessCommandM (PlayCard player card) =
  -- darf der das?
  do allowed <- isPlayCardAllowedM player card
     if allowed
     then do  recordEventM (LegalCardPlayed player card)
              turnOverTrick <- turnOverTrickM
              case turnOverTrick of
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
     else do recordEventM (IllegalCardAttempted player card)
             return Nothing

-- Gesamtes Spiel spielen
tableLoopM :: GameCommand -> Game Player
tableLoopM command =
  do maybeWinner <- tableProcessCommandM command
     case maybeWinner of
      Nothing -> GetCommand tableLoopM -- hätte gern einen Callback vom Typ (GameCommand -> Game a)
      Just winner -> return winner