{-# LANGUAGE TupleSections #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE GADTs #-}
{-# OPTIONS_GHC -fplugin=Polysemy.Plugin #-}

module EventSourcing where

import Polysemy

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

data EventSourcing state event monad a where
  -- gibt bekannt, dass ein Event passiert ist, zusammen mit Zustandstransformation
  TellEvent :: event -> (event -> state -> state)  -> EventSourcing state event monad ()
  -- Teilprogramm einklammern und Events davon extrahieren
  BracketEvents :: monad a -> EventSourcing state event monad (a, [event])
  -- Zustand extrahieren
  EventState :: EventSourcing state event monad state

makeSem ''EventSourcing

-- bracketEvents ohne Rückgabe
bracketEvents_ :: Member (EventSourcing state event) effects => Sem effects a -> Sem effects [event]
bracketEvents_ action =
  do (_, events) <- bracketEvents action
     return events

eventSourcingToStateWriter ::
  Sem (EventSourcing state event : effects) a ->
    Sem (State state : Writer [event] : effects) a
eventSourcingToStateWriter =
  reinterpret2H
    (\case
        TellEvent event transformState ->
          do Writer.tell [event]
             State.modify (transformState event)
             pureT ()
        BracketEvents inner ->
          do minner <- runT inner
             -- Sandy says this is stupid
             (events, fresult) <- raise (Writer.listen (subsume (subsume (eventSourcingToStateWriter minner))))
             pure (fmap (, events) fresult)
        EventState ->
          do state <- State.get
             pureT state)


runEventSourcing :: Sem (EventSourcing state event : effects) a -> state -> Sem effects (a, [event], state)
runEventSourcing action state =
  do let stateWriter = eventSourcingToStateWriter action
         writer = State.runState state stateWriter 
     (events, (state, result)) <- Writer.runWriter writer
     return (result, events, state)

-- total events in reverse order, bracketEvents-returned events in correct order
eventSourcingToState :: Sem (EventSourcing state event : effects) a ->
    Sem (State ([event], state) : effects) a
eventSourcingToState =
  reinterpretH
    (\case
        TellEvent event transformState ->
          do State.modify (\ (events, state) ->
                              (event : events, transformState event state))
             pureT ()
        BracketEvents inner ->
          do minner <- runT inner
             (events, state) <- State.get
             State.put ([], state)
             fresult <- raise (subsume (eventSourcingToState minner))
             (newEvents, state) <- State.get
             State.put (newEvents ++ events, state)
             pure (fmap (, reverse newEvents) fresult)
        EventState ->
          do (events, state) <- State.get
             pureT state)

eventSourcingToIO :: IORef ([event], state) -> Sem '[EventSourcing state event] a -> Sem '[Embed IO] a
eventSourcingToIO ref action =
  State.runStateIORef ref (raiseUnder (eventSourcingToState action))

p1 :: Sem '[EventSourcing String Int] (String, [Int])
p1 = do let transform = (\ event state -> (show event) ++ " " ++ state)
        tellEvent 4 transform
        (result, events) <- bracketEvents (do tellEvent 3 transform
                                              tellEvent 2 transform
                                              return "result")
        tellEvent 5 transform
        return (result, events)

p1run = run (runEventSourcing p1 "start")

