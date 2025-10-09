{-# LANGUAGE InstanceSigs #-}
module GameEvent where

import Cards

import qualified Data.Set as Set
import Data.Set (Set)

import qualified Data.Map as Map
import Data.Map (Map)

-- Event: Repräsentation eines Ereignisses (das passiert ist)

-- Event-Sourcing: Events bilden bestandsführende System

-- - in der Vergangenheit
-- - fachlich, nicht technisch (Fakt)
-- - müssen ganze Geschichte, enthalten alle bekannte Information
-- - Redunanz
-- - sollten nicht resultierenden Zustand enthalten
{-
data GameEvent =
    CardsDealt [(Player, Set Card)]
  | TwoOfClubsPlayed Player
  | CardPlayed Player Card
  | TurnChanged Player
  | TrickTaken Player (Set Card)
  | CardsPassed Player (Set Card)
  | GameWon Player
-}

data GameEvent
  = HandDealt Player Hand
  | PlayerTurnChanged Player
  | LegalCardPlayed Player Card
  | TrickTaken Player Trick
  | IllegalCardAttempted Player Card
  | GameEnded Player
  deriving (Show, Eq)

-- Command: Wunsch, daß etwas passieren möge
-- - in der Zukunft
-- - passiert nicht unbedingt
data GameCommand =
    PlayCard Player Card
  | DealHands (Map Player Hand)
  deriving (Show, Eq)

data Game a =
    RecordEvent GameEvent (() -> Game a)
  | IsPlayValid Player Card (Bool -> Game a)
  | RoundOverTrick (Maybe (Trick, Player) -> Game a)
  | PlayerAfter Player (Player -> Game a)
  | GameOver (Maybe Player -> Game a)
  | GetCommand (GameCommand -> Game a)
  | Return a

recordEventM :: GameEvent -> Game ()
recordEventM event = RecordEvent event Return

isPlayValidM :: Player -> Card -> Game Bool
isPlayValidM player card = IsPlayValid player card Return

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
    (>>=) (Return result) next = next result
    (>>=) (RecordEvent event callback) next =
        RecordEvent event (\() ->
            callback () >>= next)
    (>>=) (IsPlayValid player card callback) next =
        IsPlayValid player card (\valid ->
            callback valid >>= next)
    (>>=) (RoundOverTrick cont) next =
        RoundOverTrick ( \over ->
          cont over >>= next)
    (>>=) (PlayerAfter player cont) next =
        PlayerAfter player ( \player ->
          cont player >>= next)
    (>>=) (GameOver cont) next =
        GameOver ( \won ->
          cont won >>= next)
    (>>=) (GetCommand cont) next =
        GetCommand (\command -> cont command >>= next)

-- Just winner, wenn das Spiel vorbei
-- muß Events generieren
tableProcessCommand :: GameCommand -> Game (Maybe Player)
tableProcessCommand (DealHands hands) =
    let pairs = Map.toList hands
        events = map (uncurry HandDealt) pairs
    in do mapM_ recordEventM events
          return Nothing -- Spiel noch nicht vorbei
tableProcessCommand (PlayCard player card) = 
    do valid <- isPlayValidM player card
       if valid
       then do recordEventM (LegalCardPlayed player card)
               roundOverTrick <- roundOverTrickM
               case roundOverTrick of
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
                    do
                    nextPlayer <- playerAfterM player
                    recordEventM (PlayerTurnChanged nextPlayer)
                    return Nothing

       else do recordEventM (IllegalCardAttempted player card)
               return Nothing

-- vom DealHands bis zum Ende spielen
tableLoopM :: GameCommand -> Game Player
tableLoopM command =
    do maybeWinner <- tableProcessCommand command
       case maybeWinner of
        Nothing -> GetCommand tableLoopM
        Just winner -> return winner