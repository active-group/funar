{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs, FlexibleContexts, TypeOperators, DataKinds, PolyKinds #-}
{-# LANGUAGE LambdaCase, BlockArguments, TypeApplications, ScopedTypeVariables, ConstraintKinds #-}
{-# OPTIONS_GHC -fplugin=Polysemy.Plugin #-}
module TableEventSourcing where

import qualified Data.Map.Strict as Map
import Data.Map.Strict (Map, (!))

import qualified Data.Set as Set
import Data.Set (Set)

import qualified Data.IORef as IORef
import Data.IORef (IORef)

import Polysemy
import qualified Polysemy.State as State
import Polysemy.State (State)

import Cards
import GameEvent
import Table
import EventSourcing

type GameEventSourcing = EventSourcing TableState GameEvent

-- Monadische Versionen von Zustands-Zugriffen

playerHandM :: Member GameEventSourcing effects => Player -> Sem effects Hand
playerHandM player =
  do state <- eventState
     return (tableStateHands state ! player)


playerStackM :: Member GameEventSourcing effects => Player -> Sem effects Stack
playerStackM player =
  do state <- eventState
     return (tableStateStacks state ! player)

trickM :: Member GameEventSourcing effects => Sem effects Trick
trickM = fmap tableStateTrick eventState

processGameEventM :: Member GameEventSourcing effects => GameEvent -> Sem effects ()
processGameEventM event =
  tellEvent event tableProcessEvent

whoTakesTrickM :: Member GameEventSourcing effects => Sem effects (Player, Trick)
whoTakesTrickM = do
  state <- eventState
  let trick = tableStateTrick state
  return (whoTakesTrick trick, trick)

turnOverM :: Member GameEventSourcing effects => Sem effects Bool
turnOverM = fmap turnOver eventState 

gameOverM :: Member GameEventSourcing effects => Sem effects (Maybe Player)
gameOverM = fmap gameOver eventState

playValidM :: Member GameEventSourcing effects => Player -> Card -> Sem effects Bool
playValidM player card  =
  do state <- eventState
     return (playValid state player card)

currentTrickM :: Member GameEventSourcing effects => Sem effects Trick
currentTrickM = fmap tableStateTrick eventState

processGameCommandM :: Member GameEventSourcing effects => GameCommand -> Sem effects ()
processGameCommandM command =
  do state <- eventState
     let events = tableProcessCommand command state
     mapM_ processGameEventM events

gameCommandEventsM ::
  Member GameEventSourcing effects => GameCommand -> Sem effects [GameEvent]
-- gameCommandEventsM gameCommand | trace ("gameCommandsEventsM " ++ show gameCommand) False = undefined
gameCommandEventsM gameCommand =
  do gameState <- eventState
     gameEvents <- bracketEvents_ (processGameCommandM gameCommand)
     return gameEvents

gameCommandEventsIO :: [Player] -> IO (GameCommand -> IO [GameEvent])
gameCommandEventsIO players =
  do ref <- IORef.newIORef ([], emptyGameState players)
     let gameCommandEventsMIO gameCommand =
           runM (eventSourcingToIO ref (bracketEvents_ (processGameCommandM gameCommand)))
     return gameCommandEventsMIO

