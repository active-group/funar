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

data Game a
  = PlayValid Player Card (Bool -> Game a)
  | RecordEvent GameEvent (() -> Game a)
  | TurnOverTrick (Maybe (Trick, Player) -> Game a)
  | PlayerAfter Player (Player -> Game a)
  | GameOver (Maybe Player -> Game a)
  | Done a

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
    PlayValid
      player
      card
      ( \valid ->
          cont valid >>= next
      )
  (RecordEvent event cont) >>= next =
    RecordEvent
      event
      ( \() ->
          cont () >>= next
      )
  (TurnOverTrick cont) >>= next =
    TurnOverTrick
      ( \over ->
          cont over >>= next
      )
  (PlayerAfter player cont) >>= next =
    PlayerAfter
      player
      ( \player ->
          cont player >>= next
      )
  (GameOver cont) >>= next =
    GameOver
      ( \won ->
          cont won >>= next
      )
  (Done result) >>= next = next result


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
