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

module GameIO where

import Data.IORef (IORef)
import qualified Data.IORef as IORef
import Polysemy
import Polysemy.Cont
import Polysemy.Internal (send)
import Polysemy.State (State)
import qualified Polysemy.State as State

import Cards
import GameEvent
import Table
import Game

gameIO :: [Player] -> IO (GameCommand -> IO [GameEvent])
gameIO players =
  do
    contRef <-
      IORef.newIORef
        ( Just
            ( \command ->
                do
                  let game = playLoop command
                  let game' = State.evalState (emptyTableState players) game
                  let gameOne =
                        callCC
                          ( \cont ->
                              fmap GameDone (Game.playOne cont game')
                          )
                  return (run (runContPure gameOne))
            )
        )
    -- loop until we get to WaitingForCommand or GameDone
    let gatherEvents' stepResult events =
          case stepResult of
            EventBroadcast event cont ->
              do
                let stepResult' = run (runContPure (cont ()))
                gatherEvents' stepResult' (events ++ [event])
            WaitingForCommand cont ->
                return (events, Just cont)
            GameDone _ -> return (events, Nothing)

    let process command =
          do
            mbCont <- IORef.readIORef contRef
            case mbCont of
              Nothing -> return []
              Just cont ->
                  do let stepResult = run (runContPure (cont command))
                     (events, mbCont') <- gatherEvents' stepResult []
                     IORef.writeIORef contRef mbCont'
                     return events
    return process
