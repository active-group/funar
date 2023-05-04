module GameEvent where

import Cards

import qualified Data.Map as Map
import Data.Map (Map)

-- Event: Objekt, das ein Ereignis beschreibt, das bereits passiert ist.

-- - in der Vergangenheit
-- - sollte möglichst alle bekannten Informationen enthalten
-- - "die ganze Geschichte der Domäne erzählen"
-- - Redundanz ist OK
-- - (laß den Zustand da raus)
{-
data GameEvent =
    CardPlayed Card Player
  | CardsDistributed [(Player, Hand)]
  | TrickTaken Player Trick
  | GameFinished Player
  | RoundFinished [(Player, Integer)] -- Punkte pro Spieler
-}

data GameEvent
  = HandDealt Player Hand
  | PlayerTurnChanged Player
  | LegalCardPlayed Player Card
  | TrickTaken Player Trick
  | IllegalCardAttempted Player Card
  | GameEnded Player
  deriving (Show, Eq)


-- vs. Command (optional)
-- Objekt, das einen Wunsch beschreibt, dass etwas passieren möge
-- != Event

{-
data GameCommands =
    PlayCard Card Player Integer -- welche Position im Stich
-}

data GameCommand
  = DealHands (Map Player Hand)
  | PlayCard Player Card
  deriving (Show, Eq)

-- Steht in den Events, wer gewonnen hat?
eventsWinner :: [GameEvent] -> Maybe Player
eventsWinner [] = Nothing
eventsWinner (first : rest) =
  case first of
    GameEnded winner -> Just winner
    _ -> eventsWinner rest

data Game a =
    RecordEvent GameEvent (() -> Game a)
  | PlayValid Player Card (Bool -> Game a)
  | TurnOverTrick (Maybe (Trick, Player) -> Game a)
  | Done a

recordEventM :: GameEvent -> Game ()
recordEventM event = RecordEvent event Done

playValidM :: Player -> Card -> Game Bool
playValidM player card = PlayValid player card Done

turnOverTrickM :: Game (Maybe (Trick, Player))
turnOverTrickM = TurnOverTrick Done

instance Functor Game where

instance Applicative Game where

instance Monad Game where
    return = Done
    (>>=) (RecordEvent event callback) next = 
        RecordEvent event (\() -> 
            -- (>>=) (callback ()) next)
            callback () >>= next)
    (>>=) (PlayValid player card callback) next =
        PlayValid player card (\isValid ->
            callback isValid >>= next)
    (>>=) (TurnOverTrick callback) next =
        TurnOverTrick (\maybeTrick ->
            callback maybeTrick >>= next)
    (>>=) (Done result) next = next result

-- data Maybe a = Nothing | Just a

-- einen Spielschritt voranschreiten
tableProcessCommandM :: GameCommand -> Game (Maybe Player) -- ist das Spiel zu Ende und wenn ja, wer gewonnen?
tableProcessCommandM (DealHands hands) =
    let events = map (uncurry HandDealt) (Map.toList hands)
    -- mapM_ :: Monad m => (a -> m b) -> [a] -> m ()
    in do mapM_ recordEventM events
          return Nothing
tableProcessCommandM (PlayCard player card) =
    do isValid <- playValidM player card
       if isValid 
       then 
        do recordEventM (LegalCardPlayed player card)
           maybeTrick <- turnOverTrickM
           case maybeTrick of
            Just (trick, trickTaker) -> undefined
            Nothing -> undefined
       else 
        do recordEventM (IllegalCardAttempted player card)
           return Nothing
