module GameEvent where

import Cards

import qualified Data.Map as Map
import Data.Map (Map)

-- Event: Objekt, das ein fachliches Ereignis in der Domäne repräsentiert.

-- - in der Vergangenheit passiert
-- - können gesamte Geschichte der Domäne erzählen
-- - Redundanz ist OK

-- Command: Objekt, das einen (fachlichen) Wunsch repräsentiert.

-- - in der Zukunft
-- - != Event
{-
type RandomSeed = Integer

data GameEvent =
    GameStarted [Player]
  | FirstMoveMade Player
  | DeckShuffled RandomSeed
  | CardsDistributed Hand Hand Hand Hand
  | CardPlayed Player Card
  | RoundFinished Player Trick
  | PilesFinished Pile Pile Pile Pile
  | GameFinished Player
  | TurnChanged Player 

data GameCommand =
    PlayCard Player Card
  | MakeFirstMove Player
  | StartGame [Card] -- Hand Hand Hand Hand
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

data Game a =
    RecordEvent GameEvent (() -> Game a)
  | PlayValid Player Card (Bool -> Game a)
  | TurnOverTrick (Maybe (Trick, Player) -> Game a)
  | PlayerAfter Player (Player -> Game a)
  | GameOver (Maybe Player -> Game a)
  | Done a

instance Functor Game where

instance Applicative Game where

instance Monad Game where
    return = Done
    (>>=) (RecordEvent event callback) next =
        RecordEvent event (\() ->
            callback () >>= next)
    (PlayValid player card callback) >>= next =
        PlayValid player card (\valid ->
            callback valid >>= next)
    (TurnOverTrick callback) >>= next =
        TurnOverTrick (\over -> callback over >>= next)
    (PlayerAfter player callback) >>= next =
        PlayerAfter player (\nextPlayer -> callback nextPlayer >>= next)
    (GameOver callback) >>= next =
        GameOver (\over -> callback over >>= next)
    (Done result) >>= next = next result
 
recordEventM :: GameEvent -> Game ()
recordEventM event = RecordEvent event Done

-- sagt uns, ob ein Spielzug zulässig ist
playValidM :: Player -> Card -> Game Bool
playValidM player card = PlayValid player card Done

-- ist die Runde vorbei und wenn ja, was ist der Stich und wer muß ihn nehmen
turnOverTrickM :: Game (Maybe (Trick, Player))
turnOverTrickM = TurnOverTrick Done

-- wer ist Spieler dran?
playerAfterM :: Player -> Game Player
playerAfterM player = PlayerAfter player Done

gameOverM :: Game (Maybe Player)
gameOverM = GameOver Done

-- liefert, wer gewonnen hat
tableProcessCommandM :: GameCommand -> Game (Maybe Player)
tableProcessCommandM (DealHands hands) = 
    do mapM_ (\(player, hand) -> recordEventM (HandDealt player hand)) 
             (Map.toList hands)
       return Nothing

tableProcessCommandM (PlayCard player card) =
    do valid <- playValidM player card
       if valid
       then do recordEventM (LegalCardPlayed player card)
               over <- turnOverTrickM
               case over of
                Nothing -> 
                     do nextPlayer <- playerAfterM player
                        recordEventM (PlayerTurnChanged nextPlayer)
                        return Nothing
                Just (trick, trickTaker) ->
                    do gameOver <- gameOverM
                       case gameOver of
                        Nothing ->
                            do recordEventM (PlayerTurnChanged trickTaker)
                               return Nothing
                        Just winner ->
                            do recordEventM (GameEnded winner)
                               return (Just winner)
                       return undefined
       else 
        do recordEventM (IllegalCardAttempted player card)
           return Nothing