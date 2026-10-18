{-# LANGUAGE FlexibleInstances #-}

module Player where

import Cards
import Data.IORef (IORef)
import qualified Data.IORef as IORef
import Data.Map.Strict (Map, (!))
import qualified Data.Map.Strict as Map
import Data.Set (Set)
import qualified Data.Set as Set
import Debug.Trace (trace)
import Free
import GameEvent (GameCommand (..), GameEvent (..))

-- Spieler kennt das, was er sieht (und sich selbst)
data PlayerState = PlayerState
  { playerStateHand :: Hand,
    playerStateTrick :: Trick,
    playerStatePile :: Pile,
    playerStatePlayer :: Player
  }
  deriving (Show)

makeEmptyPlayerStateFor = PlayerState emptyHand emptyTrick emptyPile

-- Spielevent "sehen"
playerProcessEvent :: GameEvent -> PlayerState -> PlayerState
playerProcessEvent (HandDealt player' hand) state =
  if playerStatePlayer state == player'
    then
      state
        { playerStateHand = hand,
          playerStateTrick = emptyTrick,
          playerStatePile = emptyPile
        }
    else state
playerProcessEvent (PlayerTurnChanged player') state = state
playerProcessEvent (LegalCardPlayed player' card) state =
  if playerStatePlayer state == player'
    then
      state
        { playerStateHand = removeCard card (playerStateHand state),
          playerStateTrick = addToTrick player' card (playerStateTrick state)
        }
    else state {playerStateTrick = addToTrick player' card (playerStateTrick state)}
playerProcessEvent (TrickTaken player' trick) state =
  if playerStatePlayer state == player'
    then
      state
        { playerStateTrick = emptyTrick,
          playerStatePile = pileAddTrick (playerStatePile state) trick
        }
    else state {playerStateTrick = emptyTrick}
playerProcessEvent (IllegalCardAttempted player' card) state = state
playerProcessEvent (GameEnded winner) state = state

data StatePlayerOps m =
  StatePlayerOps {
     getEventOp :: m GameEvent,
     recordCommandOp :: GameCommand -> m (),
     getPlayerStateOp :: m PlayerState
  }

chooserStrategy ::
  Monad m =>
  StatePlayerOps m ->
  Player ->
  m Card ->
  m ()
chooserStrategy ops player choose =
  do
    event <- getEventOp ops
    state <- getPlayerStateOp ops
    case event of
      HandDealt player' hand ->
        if (player == player') && (containsCard (Card Clubs Two) hand)
          then recordCommandOp ops (PlayCard player (Card Clubs Two))
          else return ()
      PlayerTurnChanged player' ->
        if player == player'
          then do
            card <- choose
            recordCommandOp ops (PlayCard player card)
          else return ()
      LegalCardPlayed player' card -> return ()
      IllegalCardAttempted player' card -> return ()
      TrickTaken player' trick -> return ()
      GameEnded winner -> return ()
    chooserStrategy ops player choose



