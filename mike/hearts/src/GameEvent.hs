{-# LANGUAGE InstanceSigs #-}
module GameEvent where

import Cards

import qualified Data.Map as Map
import Data.Map (Map)

-- event: representation of something that happened
-- ... in the past (it's a fact)
-- ... in the domain
-- ... redundancy is OK
-- ... doesn't contain the state
-- event sourcing: events tell the entire story of the domain
-- ... contain everything about what happened

data GameEvent
  = HandDealt Player Hand
  | PlayerTurnChanged Player
  | LegalCardPlayed Player Card
  | TrickTaken Player Trick
  | IllegalCardAttempted Player Card
  | GameEnded Player
  deriving (Show, Eq)

-- command: representation of something somebody wants to happen
-- ... in the future
-- ... might not happen
data GameCommand
  = DealHands (Map Player Hand)
  | PlayCard Player Card
  deriving (Show, Eq)

-- game monad
data Game a =
    RecordEvent GameEvent (() -> Game a)
  | IsCardLegal Player Card (Bool -> Game a)
  | RoundOverTrick (Maybe (Trick, Player) -> Game a)
  | PlayerAfter Player (Player -> Game a)
  | GameOver (Maybe Player -> Game a)
  | GetCommand (GameCommand -> Game a)
  | Return a

recordEventM :: GameEvent -> Game ()
recordEventM event = RecordEvent event Return

isCardLegalM :: Player -> Card -> Game Bool
isCardLegalM player card = IsCardLegal player card Return

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
  (>>=) (RecordEvent event cont) next =
    RecordEvent event (\() -> cont () >>= next)
  (>>=) (IsCardLegal player card cont) next =
    IsCardLegal player card (\legal -> cont legal >>= next)
  (>>=) (RoundOverTrick cont) next =
    RoundOverTrick (\stuff -> cont stuff >>= next)
  (>>=) (PlayerAfter player cont) next =
    PlayerAfter player (\player -> cont player >>= next)
  (>>=) (GameOver cont) next =
    GameOver (\winner -> cont winner >>= next)
  (>>=) (GetCommand cont) next =
    GetCommand (\command -> cont command >>= next)

-- player is the player who won if the game is over
tableProcessCommandM :: GameCommand -> Game (Maybe Player)
tableProcessCommandM (DealHands hands) =
  -- generate HandDealt events
  let pairs = Map.toList hands
      handDealts = map (uncurry HandDealt) pairs
      ms = map recordEventM handDealts
  in do sequence_ ms
        return Nothing -- game is not over
tableProcessCommandM (PlayCard player card) =
  -- check whether the card is legal
  do legal <- isCardLegalM player card
     if legal
     then do roundOverTrick <- roundOverTrickM
             case roundOverTrick of
              Nothing ->
                do next <- playerAfterM player
                   recordEventM (PlayerTurnChanged next)
                   return Nothing
              Just (trick, trickTaker) ->
                do recordEventM (TrickTaken trickTaker trick)
                   over <- gameOverM
                   case over of
                    Nothing -> 
                      do recordEventM (PlayerTurnChanged trickTaker)
                         return Nothing
                    Just winner ->
                      do recordEventM (GameEnded winner)
                         return (Just winner)
     else do recordEventM (IllegalCardAttempted player card)
             return Nothing

tableLoopM :: GameCommand -> Game Player
tableLoopM command =
  do maybeWinner <- tableProcessCommandM command
     case maybeWinner of
      Nothing -> GetCommand tableLoopM
      Just winner -> return winner