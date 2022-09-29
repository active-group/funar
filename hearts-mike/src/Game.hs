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

import GameEvent
import Cards
import Table

import Debug.Trace (trace)

type GameStepState = (GameCommand -> Game (Maybe Player), TableState)

type GameStepEffects effects = Member (State GameStepState) effects

initialGameStepState :: GameStepState
initialGameStepState = undefined

gameSem :: GameStepEffects effects => [Player] -> Sem effects (GameCommand -> Sem effects [GameEvent])
gameSem players =
  do
    State.put (tableLoopM, emptyTableState players)
    let processCommand command =
          do
            (next, state) <- State.get
            let (step, state') = runGameStep (next command) state
            let loop step state events =
                  case step of
                    RecordedEvent event cont ->
                      do
                        let (step', state') = trace ("RecordedEvent " ++ show event) (runGameStep (cont ()) state)
                        loop step' state' (event : events)
                    NeedsCommand cont ->
                      do
                        State.put (cont, state)
                        return (reverse events)
                    GameDone result ->
                      return (reverse events)
            loop step state' []
    return processCommand
