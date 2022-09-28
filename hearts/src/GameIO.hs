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
import Polysemy.State (State)
import qualified Polysemy.State as State

import Cards
import GameEvent
import Table
import Game

gameIO :: [Player] -> IO (GameCommand -> IO [GameEvent])
{-
gameIO players =
  do ref <- IORef.newIORef (tableLoopM, emptyTableState players)
     let processCommand command =          
           do (next, state) <- IORef.readIORef ref
              putStrLn ("processCommand " ++ (show command))
              let (step, state') = runGameStep (next command) state              
              let loop step state events =
                    case step of
                      RecordedEvent event cont ->
                        do putStrLn ("RecordedEvent " ++ show event)
                           let (step', state') = runGameStep (cont ()) state
                           loop step' state' (event:events)
                      NeedsCommand cont ->
                        do putStrLn ("NeedsCommand")
                           IORef.writeIORef ref (cont, state)
                           return (reverse events)
                      GameDone result ->
                        return (reverse events)
              loop step state' []
     return processCommand
-}
gameIO players =
  do ref <- IORef.newIORef (tableLoopM, emptyTableState players)
     let game = gameSem @'[State GameStepState, Embed IO] players
     let gameIOSem = State.runStateIORef ref game
     game <- runM gameIOSem
     let gameIO command =
          runM (State.runStateIORef ref (game command))            
     return gameIO