{-# LANGUAGE InstanceSigs #-}
module GameEvent where

import Cards

import qualified Data.Map as Map
import Data.Map (Map)

{-
Events:
- Beschreibung eines Ereignisses in der Vergangenheit
- erzählen Geschichte der Domäne - die gesamte Geschichte
- alles, was wir über ein Ereignis weiß
- Redundanz OK
- fachlich
- enthält nicht den Zustand

vs. Commands (optional)
- Beschreibung eines Wunsches, in der Zukunft
-}

{-
data GameEvent =
    CardPlayed Card Player
  | CardsShuffled [Card]
  | GameStarted [Player]
  | GameEnded Player
  | RoundEnded Player Trick
  | CardsDealt (Map Player Hand)
  | PlayerTurnChanged Player

data GameCommand =
    Cheat Player Card
  | PlayCard Player Card
  | TakeTrick Player Trick
  | StartGame [Player]
  | ShuffleCards [Card]
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
    Return a
  | RecordEvent GameEvent (() -> Game a)
  | IsCardOk Player Card (Bool -> Game a)
  | TurnOverTrick (Maybe (Trick, Player) -> Game a)
  | PlayerAfter Player (Player -> Game a)
  | GameOver (Maybe Player -> Game a)
  | GetCommand (GameCommand -> Game a)

recordEventM :: GameEvent -> Game ()
recordEventM event = RecordEvent event Return

isCardOkM :: Player -> Card -> Game Bool
isCardOkM player card = IsCardOk player card Return

turnOverTrickM :: Game (Maybe (Trick, Player))
turnOverTrickM = TurnOverTrick Return

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
    (>>=) (Return result) next = next result
    (>>=) (RecordEvent event cont) next =
        RecordEvent event (\() -> cont () >>= next)
    (>>=) (IsCardOk player card cont) next =
        IsCardOk player card (\ok -> cont ok >>= next)
    (>>=) (TurnOverTrick cont) next =
        TurnOverTrick ( \over -> cont over >>= next)
    (>>=) (PlayerAfter player cont) next =
        PlayerAfter player( \player -> cont player >>= next)
    (>>=) (GameOver cont) next =
        GameOver ( \won -> cont won >>= next)
    (>>=) (GetCommand cont) next =
        GetCommand (\command -> cont command >>= next)

-- data Maybe a = Just a | Nothing

-- Spielregeln: GameCommand rein, GameEvents raus
tableProcessCommandM :: GameCommand -> Game (Maybe Player)
tableProcessCommandM (DealHands hands) =
    let gameMs = fmap (recordEventM . uncurry HandDealt) (Map.toList hands)
    in do sequence_ gameMs
          return Nothing
tableProcessCommandM (PlayCard player card) =
    do ok <- isCardOkM player card
       if ok
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
