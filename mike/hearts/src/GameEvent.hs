module GameEvent where

import Cards

import qualified Data.Map as Map
import Data.Map (Map)

{-
Event: Objekt, das ein Ereignis beschreibt

- ... Ereignis in der Domäne
- ... ist (in der Vergangenheit) passiert
- ... sollten alles beschreiben, was passiert
- ... Redundanz ist OK

Command: Objekt, das einen Wunsch beschreibt
- ... ist noch nicht passiert (=> in der Zukunft)

-}
{-
data GameEvent =
    CardsShuffled [Card]
  | CardsDealt Player (Map Player Hand)
  | CardPlayed Player Card
  | TrickDone Player Trick
  | GameEnded Player
  deriving Show

data GameCommand =
      Shuffle [Card]
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

-- Steht in den Events, wer gewonnen hat?
eventsWinner :: [GameEvent] -> Maybe Player
eventsWinner [] = Nothing
eventsWinner (first : rest) =
  case first of
    GameEnded winner -> Just winner
    _ -> eventsWinner rest

-- Monade für den Spielablauf
data Game a =
    RecordEvent GameEvent (() -> Game a) 
  | PlayValid Player Card (Bool -> Game a)
  | TurnOverTrick (Maybe (Player, Trick) -> Game a)
  | PlayerAfter Player (Player -> Game a)
  | GameOver (Maybe Player -> Game a)
  | GetCommand (GameCommand -> Game a)
  | Done a

recordEventM :: GameEvent -> Game ()
recordEventM event = RecordEvent event Done

playValidM :: Player -> Card -> Game Bool
playValidM player card = PlayValid player card Done

turnOverTrickM :: Game (Maybe (Player, Trick))
turnOverTrickM = TurnOverTrick Done

playerAfterM :: Player -> Game Player
playerAfterM player = PlayerAfter player Done

gameOverM :: Game (Maybe Player)
gameOverM = GameOver Done

instance Functor Game where

instance Applicative Game where

instance Monad Game where
    return = Done

    (>>=) (RecordEvent event callback) next =
      RecordEvent event (\() -> callback () >>= next )
    (>>=) (PlayValid player card callback) next =
      PlayValid player card (\ valid -> callback valid >>= next)
    (>>=) (TurnOverTrick callback) next =
      TurnOverTrick (\ playerTrick -> callback playerTrick >>= next)
    (>>=) (GameOver callback) next =
      GameOver (\ over -> callback over >>= next)
    (>>=) (PlayerAfter player callback) next =
      PlayerAfter player (\player' -> callback player' >>= next)
    (>>=) (GetCommand callback) next =
      GetCommand (\command -> callback command >>= next)
    (>>=) (Done result) next = next result


tableProcessCommandM :: GameCommand -> Game (Maybe Player)
tableProcessCommandM (DealHands hands) =
    do mapM_ (recordEventM . uncurry HandDealt) (Map.toList hands)
       return Nothing
tableProcessCommandM (PlayCard player card) =
  do
    valid <- playValidM player card
    if valid
      then do
        recordEventM (LegalCardPlayed player card)
        turnOverTrick <- turnOverTrickM
        case turnOverTrick of
          Just (trickTaker, trick) ->
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
-- gesamtes Spiel
tableLoopM :: GameCommand -> Game Player
tableLoopM command =
    do maybeWinner <- tableProcessCommandM command
       case maybeWinner of
        Nothing ->
            GetCommand tableLoopM
        Just winner ->
            return winner