module GameEvent where

import Cards

import qualified Data.Map as Map
import Data.Map (Map)

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

-- | Steht in den Events, wer gewonnen hat?
-- >>> let mike = Player "Mike"
-- >>> let peter = Player "Peter"
-- >>> eventsWinner []
-- Nothing
-- >>> eventsWinner [PlayerTurnChanged mike]
-- Nothing
-- >>> eventsWinner [PlayerTurnChanged peter, GameEnded mike]
-- Just (Player {playerName = "Mike"})
eventsWinner :: [GameEvent] -> Maybe Player
eventsWinner [] = Nothing
eventsWinner (first : rest) =
  case first of
    GameEnded winner -> Just winner
    _ -> eventsWinner rest

data Game a =
   PlayValid Player Card (Bool -> Game a)
 | RecordEvent GameEvent (() -> Game a)
 | GetCommand (GameCommand -> Game a)
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

instance Functor Game

instance Applicative Game

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
  (GetCommand cont) >>= next =
    GetCommand
      ( \command ->
          cont command >>= next
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

-- | Einen Spielschritt verarbeiten
tableProcessCommandM :: GameCommand -> Game (Maybe Player)
tableProcessCommandM (DealHands hands) =
  do
    mapM_ (recordEventM . uncurry HandDealt) (Map.toList hands)
    return Nothing
tableProcessCommandM (PlayCard player card) =
  do
    valid <- playValidM player card
    if valid
      then do
        recordEventM (LegalCardPlayed player card)
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
      else do
        recordEventM (IllegalCardAttempted player card)
        return Nothing

-- | Gesamtes Spiel spielen
tableLoopM :: GameCommand -> Game Player
tableLoopM command =
  do
    maybeWinner <- tableProcessCommandM command
    case maybeWinner of
      Nothing ->
        GetCommand tableLoopM
      Just winner ->
        return winner
