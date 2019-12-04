{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE UndecidableInstances #-}

module EventSourcing where

import qualified Control.Monad.State.Lazy as State
import Control.Monad.State.Lazy (State, StateT, MonadState)
import Control.Monad.Trans.Class
import Control.Monad.Trans.Free
import Data.Functor.Identity

import qualified Control.Monad.Writer (WriterT)
import Control.Monad.Writer as Writer

import Data.Foldable as Foldable

import qualified Data.IORef as IORef
import Data.IORef (IORef)

import qualified Control.Monad as Monad

import Cards
import Shuffle
import Game

import qualified Data.Map.Strict as Map
import Data.Map.Strict (Map, (!))
import qualified Data.Set as Set
import Data.Set (Set)

import Debug.Trace (trace)

class Monad monad => MonadEventSourcing state event monad | monad -> state, monad -> event where
  -- gibt bekannt, dass ein Event passiert ist, zusammen mit Zustandstransformation
  tellEvent :: event -> (event -> state -> state)  -> monad ()
  -- Teilprogramm einklammern und Events davon extrahieren
  bracketEvents :: monad a -> monad (a, [event])
  -- Zustand extrahieren
  eventState :: monad state

-- bracketEvents ohne Rückgabe
bracketEvents_ :: MonadEventSourcing state event monad => monad a -> monad [event]
bracketEvents_ action =
  do (_, events) <- bracketEvents action
     return events

-- Implementierung von MonadEventSourcing
newtype EventSourcingT state event monad a =
  -- die Events sind in umgekehrter Reihenfolge
  EventSourcingT (state -> monad (a, [event], state))
  deriving Functor

-- wird benötigt für Monad
instance Monad monad => Applicative (EventSourcingT state event monad) where
  pure result = EventSourcingT (\ state -> return (result, [], state))
  (EventSourcingT ff) <*> (EventSourcingT fa) =
    EventSourcingT (\ state0 ->
                     do (f, fevents, state1) <- ff state0
                        (a, aevents, state2) <- fa state1
                        return (f a, aevents ++ fevents, state2))

instance Monad monad => Monad (EventSourcingT state event monad) where
  (EventSourcingT fm) >>= f =
    EventSourcingT (\ state0 ->
                      do (r, mevents, state1) <- fm state0
                         let EventSourcingT ffr = f r
                         (result, fevents, state2) <- ffr state1
                         return (result, fevents ++ mevents, state2))

instance MonadTrans (EventSourcingT state event) where
  lift action = EventSourcingT (\ state ->
                                  do result <- action
                                     return (result, [], state))

instance Monad monad => MonadEventSourcing state event (EventSourcingT state event monad) where
  tellEvent event transformState =
    EventSourcingT (\ state ->
                      return ((), [event], transformState event state))
  bracketEvents (EventSourcingT faction) =
    EventSourcingT (\ state0 ->
                      do (result, events, state1) <- faction state0
                         return ((result, reverse events), events, state1))
  eventState = EventSourcingT (\ state -> return (state, [], state))

type GameEventSourcingT = EventSourcingT GameState GameEvent

-- Event-Sourcing laufenlassen
runEventSourcingT :: Monad monad => EventSourcingT state event monad a -> state -> monad (a, [event], state)
runEventSourcingT (EventSourcingT faction) initialState =
  do (result, events, state) <- faction initialState
     return (result, reverse events, state)

-- nur Event-Sourcing
type EventSourcing state event = EventSourcingT state event Identity
type GameEventSourcing = EventSourcing GameState GameEvent

runEventSourcing :: EventSourcing state event a -> state -> (a, [event], state)
runEventSourcing action initialState =
  runIdentity (runEventSourcingT action initialState)

-- die Events sind in umgekehrter Reihenfolge
newtype IOEventSourcing state event a = IOEventSourcing (IORef (state, [event]) -> IO a)
  deriving Functor

ioEventSourcing (IOEventSourcing f) ref = f ref

-- fragwürdig, nur zu Demo-Zwecken
runIOEventSourcing action initialState =
  do ref <- IORef.newIORef (initialState, [])
     result <- ioEventSourcing action ref
     (state, events) <- IORef.readIORef ref
     return (result, state, reverse events)

instance Applicative (IOEventSourcing state event) where
  pure x = IOEventSourcing (\ _ -> pure x)
  (IOEventSourcing ff) <*> (IOEventSourcing fa) =
    IOEventSourcing (\ ref -> (ff ref) <*> (fa ref))

instance Monad (IOEventSourcing state event) where
  (IOEventSourcing mm) >>= f =
    IOEventSourcing (\ ref ->
                       do r <- mm ref
                          let (IOEventSourcing mf) = f r
                          mf ref)

instance MonadEventSourcing state event (IOEventSourcing state event) where
  tellEvent event transformState =
    IOEventSourcing (\ ref ->
                       IORef.modifyIORef ref (\ (state, events) -> (transformState event state, event : events)))
  eventState = IOEventSourcing (\ ref ->
                                  do (state, _) <- IORef.readIORef ref
                                     return state)
  bracketEvents action = IOEventSourcing (\ ref ->
                                          do (state, events) <- IORef.readIORef ref
                                             IORef.writeIORef ref (state, [])
                                             let (IOEventSourcing faction) = action
                                             result <- faction ref
                                             (newState, newEvents) <- IORef.readIORef ref
                                             IORef.writeIORef ref (newState, newEvents ++ events)
                                             return (result, reverse newEvents))

-- braucht UndecidableInstances
instance MonadEventSourcing state event monad => MonadEventSourcing state event (StateT state' monad) where
  tellEvent event transformEvent = lift (tellEvent event transformEvent)
  bracketEvents action =
    do state0 <- State.get
       ((result, state1), events) <- lift (bracketEvents (State.runStateT action state0))
       State.put state1
       return (result, events)
  eventState = lift eventState

type MonadGameEventSourcing monad = MonadEventSourcing GameState GameEvent monad

-- Monadische Versionen von Zustands-Zugriffen

playerHandM :: MonadGameEventSourcing monad => Player -> monad Hand
playerHandM player =
  do state <- eventState
     return (gameStateHands state ! player)

playerStackM :: MonadGameEventSourcing monad => Player -> monad (Set Card)
playerStackM player =
  do state <- eventState
     return (gameStateStacks state ! player)

trickM :: MonadGameEventSourcing monad => monad Trick
trickM = fmap gameStateTrick eventState

processGameEventM :: MonadGameEventSourcing monad => GameEvent -> monad ()
processGameEventM event =
  tellEvent event processGameEvent

whoTakesTrickM :: MonadGameEventSourcing monad => monad (Player, Trick)
whoTakesTrickM = do
  state <- eventState
  let trick = gameStateTrick state
  return (whoTakesTrick trick, trick)

turnOverM :: MonadGameEventSourcing monad => monad Bool
turnOverM = fmap turnOver eventState 

gameOverM :: MonadGameEventSourcing monad => monad Bool
gameOverM = fmap gameOver eventState

playValidM :: MonadGameEventSourcing monad => Player -> Card -> monad Bool
playValidM player card  =
  do state <- eventState
     return (playValid state player card)

currentTrickM :: MonadGameEventSourcing monad => monad Trick
currentTrickM = fmap gameStateTrick eventState

nextPlayerM :: MonadGameEventSourcing monad => monad (Player)
nextPlayerM = fmap nextPlayer eventState

gameWinnerM :: MonadGameEventSourcing monad => monad (Player)
gameWinnerM = fmap gameWinner eventState

processGameCommandM :: MonadGameEventSourcing monad => GameCommand -> monad ()
processGameCommandM (DealHands playerHands) =
   mapM_ processGameEventM (map (uncurry HandDealt) (Map.toList playerHands))
processGameCommandM (PlayCard player card) =
   do playIsValid <- playValidM player card
      if playIsValid then
        do processGameEventM (LegalCardPlayed player card)
           turnIsOver <- turnOverM
           if turnIsOver then
             do trick <- currentTrickM
                let trickTaker = whoTakesTrick trick
                processGameEventM (TrickTaken trickTaker trick)
                gameIsOver <- gameOverM
                if gameIsOver
                then
                  do winner <- gameWinnerM
                     processGameEventM (GameEnded winner)
                else processGameEventM (PlayerTurnChanged trickTaker)
           else
             do nextPlayer <- nextPlayerM
                processGameEventM (PlayerTurnChanged nextPlayer)
      else
        do nextPlayer <- nextPlayerM
           processGameEventM (IllegalCardPlayed nextPlayer card)
           processGameEventM (PlayerTurnChanged nextPlayer)

gameCommandEventsM :: MonadGameEventSourcing monad => GameCommand -> monad [GameEvent]
-- gameCommandEventsM gameCommand | trace ("gameCommandsEventsM " ++ show gameCommand) False = undefined
gameCommandEventsM gameCommand =
  do gameState <- eventState
     gameEvents <- bracketEvents_ (processGameCommandM gameCommand)
     return gameEvents

gameCommandEventsM' :: [Player] -> IO (GameCommand -> IO [GameEvent])
gameCommandEventsM' players =
  do ref <- IORef.newIORef (emptyGameState players , [])
     let gameCommandEventsMIO gameCommand =
           ioEventSourcing (bracketEvents_ (processGameCommandM gameCommand)) ref
     return gameCommandEventsMIO
