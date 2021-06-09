{-# LANGUAGE TupleSections #-}
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
import GameEvent ( GameCommand, GameEvent )
import Table
import EventSourcing

type GameEventSourcing = EventSourcing GameEvent

type TableEventSourcing effects = (Member GameEventSourcing effects, Member (State TableState) effects)

-- Monadische Versionen von Zustands-Zugriffen

playerHandM :: TableEventSourcing effects => Player -> Sem effects Hand
playerHandM player =
  do state <- State.get
     return (tableStateHands state ! player)


playerPileM :: TableEventSourcing effects => Player -> Sem effects Pile
playerPileM player =
  do state <- State.get
     return (tableStatePiles state ! player)

trickM :: TableEventSourcing effects => Sem effects Trick
trickM = fmap tableStateTrick State.get

processGameEventM :: TableEventSourcing effects => GameEvent -> Sem effects ()
processGameEventM event =
  do recordEvent event
     State.modify (tableProcessEvent event)

whoTakesTrickM :: TableEventSourcing effects => Sem effects (Player, Trick)
whoTakesTrickM = 
  fmap (\ state -> (whoTakesTrick (tableStateTrick state), tableStateTrick state)) State.get

turnOverM :: TableEventSourcing effects => Sem effects Bool
turnOverM = fmap turnOver State.get

gameOverM :: TableEventSourcing effects => Sem effects (Maybe Player)
gameOverM = fmap gameOver State.get

playValidM :: TableEventSourcing effects => Player -> Card -> Sem effects Bool
playValidM player card  =
  fmap (\ state -> playValid state player card) State.get

currentTrickM :: TableEventSourcing effects => Sem effects Trick
currentTrickM = fmap tableStateTrick State.get

processGameCommandM :: TableEventSourcing effects => GameCommand -> Sem effects [GameEvent]
processGameCommandM command =
  do state <- State.get
     let events = tableProcessCommand command state
     mapM_ processGameEventM events
     return events

gameCommandEventsM ::
  TableEventSourcing effects => GameCommand -> Sem effects [GameEvent]
-- gameCommandEventsM gameCommand | trace ("gameCommandsEventsM " ++ show gameCommand) False = undefined
gameCommandEventsM gameCommand =
  do gameState <- State.get @TableState
     checkpointEvents
     processGameCommandM gameCommand
     events <- checkpointEvents
     return events

gameCommandEventsIO :: [Player] -> IO (GameCommand -> IO [GameEvent])
gameCommandEventsIO players =
  do eventsRef <- IORef.newIORef []
     stateRef <- IORef.newIORef (emptyTableState players)
     let gameCommandEventsMIO gameCommand =
           runM (eventSourcingToIO eventsRef (State.runStateIORef  stateRef (processGameCommandM gameCommand)))
     return gameCommandEventsMIO
