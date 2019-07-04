{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}

module EventSourcing where

import qualified Control.Monad.State.Lazy as State
import Control.Monad.State.Lazy (State, StateT, MonadState)
import Control.Monad.Trans.Class

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
import Data.Map.Strict (Map)
import qualified Data.Set as Set
import Data.Set (Set)

import Debug.Trace (trace)

type EventSourcingT state event monad = StateT state (WriterT [event] monad)
type MonadEventSourcing state event monad =
  (MonadState state monad, MonadWriter [event] monad)

runEventSourcing :: StateT state (Writer [event]) () -> state -> (state, [event])
runEventSourcing action state =
  Writer.runWriter (State.execStateT action state)

type MonadGameEventSourcing monad = MonadEventSourcing GameState GameEvent monad
type GameEventSourcingT monad = EventSourcingT GameState GameEvent monad

eventSourcingReadStateM :: MonadEventSourcing state event monad => monad state
eventSourcingReadStateM = State.get

playerHandM :: MonadGameEventSourcing monad => Player -> monad Hand
playerHandM player =
  do state <- eventSourcingReadStateM
     return (gameStateHands state Map.! player)

playerStackM :: MonadGameEventSourcing monad => Player -> monad (Set Card)
playerStackM player =
  do state <- eventSourcingReadStateM
     return (gameStateStacks state Map.! player)

trickM :: MonadGameEventSourcing monad => monad Trick
trickM =
  do state <- eventSourcingReadStateM
     return (gameStateTrick state)

processGameEventM :: MonadGameEventSourcing monad => GameEvent -> monad ()
processGameEventM event =
  do gameState <- State.get
     State.put (processGameEvent event gameState)
     Writer.tell [event]
     return ()

whoTakesTrickM :: MonadGameEventSourcing monad => monad (Player, Trick)
whoTakesTrickM = do
  state <- eventSourcingReadStateM
  let trick = gameStateTrick state
  return (whoTakesTrick trick, trick)

turnOverM :: MonadGameEventSourcing monad => monad Bool
turnOverM = do
  state <- eventSourcingReadStateM
  return (turnOver state)

gameOverM :: MonadGameEventSourcing monad => monad Bool
gameOverM = do
  state <- eventSourcingReadStateM
  return (gameOver state)

playValidM :: MonadGameEventSourcing monad => Player -> Card -> monad Bool
playValidM player card  =
  do state <- eventSourcingReadStateM
     return (playValid state player card)

currentTrickM :: MonadGameEventSourcing monad => monad Trick
currentTrickM =
  do state <- eventSourcingReadStateM
     return (gameStateTrick state)

nextPlayerM :: MonadGameEventSourcing monad => monad (Player)
nextPlayerM =
  do state <- eventSourcingReadStateM
     return (nextPlayer state)

gameWinnerM :: MonadGameEventSourcing monad => monad (Player)
gameWinnerM = fmap gameWinner eventSourcingReadStateM

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
  do gameState <- eventSourcingReadStateM
     let (gameState', gameEvents) = runEventSourcing (processGameCommandM gameCommand) gameState
     mapM_ processGameEventM gameEvents
     return gameEvents

gameCommandEventsM' :: [Player] -> IO (GameCommand -> IO [GameEvent])
gameCommandEventsM' players =
  do ref <- IORef.newIORef (emptyGameState players , [])
     let gameCommandEventsMIO gameCommand =
           do (gameState, gameEvents) <- IORef.readIORef ref
              let (gameState', gameEvents') = runEventSourcing (processGameCommandM gameCommand) gameState
              IORef.writeIORef ref (gameState', gameEvents ++ gameEvents')
              return gameEvents'
     return gameCommandEventsMIO
