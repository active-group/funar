{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -fplugin=Polysemy.Plugin #-}

module Game where

import Polysemy
import Polysemy.Internal (send)
import qualified Polysemy.State as State
import Polysemy.State (State)
import Polysemy.Cont

import Data.IORef (IORef)
import qualified Data.IORef as IORef
import qualified Control.Monad as Monad
import GameEvent
import Cards
import Table
import Shuffle

import qualified Data.Map.Strict as Map
import Data.Map.Strict (Map, (!))

import qualified Data.Set as Set
import Data.Set (Set)

import Debug.Trace (trace)

data GameStep monad a where
  BroadcastEvent :: GameEvent -> GameStep monad ()
  ReceiveCommand :: GameStep monad (Maybe GameCommand)

broadcastEvent :: Member GameStep effects => GameEvent -> Sem effects ()
broadcastEvent event = send (BroadcastEvent event)

receiveCommand :: Member GameStep effects => Sem effects (Maybe GameCommand)
receiveCommand = send ReceiveCommand

-- ein Event von den Spielern verarbeiten lassen
broadcastEvents ::
  (Member GameStep effects) =>
  [GameEvent] ->
  Sem effects ()
-- broadcastEvents events | trace ("broadcastEvents " ++ show events) False = undefined
broadcastEvents events =
  Monad.mapM_ broadcastEvent events

executeCommand ::
  (Member (State TableState) effects, Member GameStep effects) =>
  GameCommand ->
  Sem effects ([GameEvent], Maybe Player)
-- executeCommand command | trace ("executeCommand " ++ show command) False = undefined
executeCommand command =
  do state <- State.get
     let events = tableProcessCommand command state
     let state' = foldl (flip tableProcessEvent) state events
     State.put state'
     return (events, gameOver state')

playLoop ::
  (Member (State TableState) effects, Member GameStep effects) =>
  GameCommand ->
  Sem effects (Maybe Player)
-- playLoop command | trace ("playLoop " ++ (show command)) False = undefined
playLoop command =
  do
    (events, maybeWinner) <- executeCommand command
    broadcastEvents events
    case maybeWinner of
      Nothing ->
        do
          maybeCommand <- receiveCommand
          case maybeCommand of
            Just command -> playLoop command
            Nothing -> return Nothing
      Just winner -> 
        return (Just winner)
playGame ::
  (Member (State TableState) effects, Member GameStep effects) =>
  [Player] ->
  [Card] ->
  Sem effects (Maybe Player)
playGame players shuffledCards = do
  let hands = Map.fromList (zip players (map Set.fromList (distribute (length players) shuffledCards)))
  playLoop (DealHands hands)

data GameStepResult
  = EventBroadcast GameEvent (() -> Sem GameStepResultEffects GameStepResult)
  | WaitingForCommand (GameCommand -> Sem GameStepResultEffects GameStepResult)
  | GameDone (Maybe Player)

instance Show GameStepResult where
  show (EventBroadcast event _) = "EventBroadcast (" ++ (show event) ++ ")"
  show (WaitingForCommand _) = "WaitingForCommand"
  show (GameDone mbPlayer) = "GameDone (" ++ (show mbPlayer) ++ ")"

type GameStepResultEffects = '[GameStepResultEffect]
type GameStepResultEffect = Cont (Ref (Sem '[]) GameStepResult)

playOne :: (forall b. GameStepResult -> Sem GameStepResultEffects b) ->
   Sem (GameStep ': GameStepResultEffects) a -> Sem GameStepResultEffects a
playOne cont0 =
  interpret
    ( \step ->
        case step of
          BroadcastEvent event ->
            callCC
              ( \cont ->
                  cont0 (EventBroadcast event cont))
          ReceiveCommand ->
            callCC
              ( \cont ->
                  cont0 (WaitingForCommand (cont . Just))))

