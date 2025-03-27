{-# LANGUAGE InstanceSigs #-}
module GameEvent where

import Cards

import qualified Data.Map as Map
import Data.Map (Map)

{-
Event: Datenwert, der ein Ereignis beschreibt
- ist passiert - in der Vergangenheit
- alle Informationen über das Ereignis
- Redundanz OK
- Zustand nicht OK
- fachlich, nicht technisch

Commands: Datenwert, der einen Wunsch beschreibt, daß etwas passiert
- in der Zukunft, noch nicht passiert
-}
{-
data GameEvent =
    PlayerTurnChanged Player
  | CardPlayed Player Card
  | StartingCardPlayed Player
  | PlayerWon Player
--  | CardsShuffled [Card] -- auch OK
  | CardsShuffled Hand Hand Hand Hand
  | RoundCompleted Trick Player
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

data Game a =
    Return a
  | RecordEvent GameEvent (() -> Game a)
  | IsValid Player Card (Bool -> Game a)
  | TurnOverTrick (Maybe (Trick, Player) -> Game a)
  | PlayerAfter Player (Player -> Game a)
  | GameOver (Maybe Player -> Game a)

recordEventM :: GameEvent -> Game ()
recordEventM event = RecordEvent event Return

isValidM :: Player -> Card -> Game Bool
isValidM player card = IsValid player card Return

turnOverTrickM :: Game (Maybe (Trick, Player))
turnOverTrickM = TurnOverTrick Return

playerAfterM :: Player -> Game Player
playerAfterM player = PlayerAfter player Return

gameOverM :: Game (Maybe Player)
gameOverM = GameOver Return


instance Functor Game where
    -- later maybe
instance Applicative Game where
    -- later maybe
    pure :: a -> Game a
    pure = Return

instance Monad Game where
    return :: a -> Game a
    return = pure
    (>>=) :: Game a -> (a -> Game b) -> Game b
    (>>=) (Return result) next = next result
    (>>=) (RecordEvent event cont) next =
        RecordEvent event (\() -> cont () >>= next)
    (>>=) (IsValid player card cont) next =
        IsValid player card (\valid -> cont valid >>= next)
    (>>=) (TurnOverTrick cont) next =
        TurnOverTrick (\m -> cont m >>= next)
    (>>=) (PlayerAfter player cont) next =
        PlayerAfter player (\player -> cont player >>= next)
    (>>=) (GameOver cont) next =
        GameOver (\winner -> cont winner >>= next)

-- Maybe Player: Ist das Spiel vorbei und wer hat gewonnen?
tableProcessCommandM :: GameCommand -> Game (Maybe Player)
tableProcessCommandM (DealHands hands) =
    let pairs = Map.toList hands
        events = map (uncurry HandDealt) pairs
    in do mapM_ recordEventM events
          return Nothing
tableProcessCommandM (PlayCard player card) =
    do valid <- isValidM player card
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
        Nothing -> undefined
        Just winner ->
            return winner
