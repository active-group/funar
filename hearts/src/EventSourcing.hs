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
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -fplugin=Polysemy.Plugin #-}

module EventSourcing where

import Polysemy
import Polysemy.Internal (send)

import qualified Polysemy.Writer as Writer
import Polysemy.Writer (Writer)
import qualified Polysemy.State as State
import Polysemy.State (State)

import Data.Foldable as Foldable

import qualified Data.IORef as IORef
import Data.IORef (IORef)

import qualified Control.Monad as Monad

import qualified Data.Map.Strict as Map
import Data.Map.Strict (Map, (!))
import qualified Data.Set as Set
import Data.Set (Set)

import Debug.Trace (trace)

data EventSourcing event monad a where
  RecordEvent :: event -> EventSourcing event monad ()
  CheckpointEvents :: EventSourcing event monad [event]

makeSem ''EventSourcing

runEventSourcingState ::
  Sem (EventSourcing event : effects) a -> (Sem (State [event] : effects) a)
runEventSourcingState =
  reinterpret
    (\ program ->
      case program of
        RecordEvent event -> 
          do events <- State.get
             State.put (event : events)
        CheckpointEvents ->
          do events <- State.get
             State.put []
             return (reverse events))

eventSourcingToIO :: Member (Embed IO) effects =>
  IORef [event] -> Sem (EventSourcing event : effects) a -> Sem effects a
eventSourcingToIO ref action =
  State.runStateIORef ref (runEventSourcingState action)
