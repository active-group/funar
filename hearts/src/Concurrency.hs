{-# LANGUAGE LambdaCase, BlockArguments, ScopedTypeVariables #-}
{-# LANGUAGE GADTs, FlexibleContexts, TypeOperators, DataKinds, PolyKinds #-}
{-# LANGUAGE RankNTypes, TypeApplications #-}
module Concurrency where

import qualified Data.Map.Strict as Map
import Data.Map.Strict (Map)
import qualified Data.Set as Set
import Data.Set (Set)

import Control.Concurrent (MVar)
import qualified Control.Concurrent as Concurrent
import qualified MVar

import Polysemy
import qualified Polysemy.State as State
import Polysemy.State (State)

import Teletype

import qualified Data.IORef as IORef
import Data.IORef (IORef)

import Cards
import GameEvent
import Table
import Player
import EventSourcing
import TableEventSourcing
import Shuffle

data ConcurrentPlayer = ConcurrentPlayer Player (MVar (GameEvent, MVar [GameCommand]))

concurrentPlayer :: Player -> Strategy '[Embed IO] -> IO ConcurrentPlayer
concurrentPlayer player strategy =
  do events <- Concurrent.newEmptyMVar
     let recur =
           do  (event, commandsChannel) <- Concurrent.takeMVar events
               commands <- runM (strategy player event)
               Concurrent.putMVar commandsChannel commands
               recur
     threadId <- Concurrent.forkIO recur
     return (ConcurrentPlayer player events)

playEventC :: Member MVar.MVarConcurrency effects => [ConcurrentPlayer] -> GameEvent -> Sem effects [GameCommand]
playEventC concurrentPlayers event =
  do commandsChannels <- mapM (\ (ConcurrentPlayer player events) ->
                                 do commandsChannel <- MVar.newEmptyMVar
                                    MVar.putMVar events (event, commandsChannel)
                                    return commandsChannel)
                              concurrentPlayers
     commandss <- mapM MVar.takeMVar commandsChannels
     return (concat commandss)


-- playCommandC :: [ConcurrentPlayer] -> GameCommand -> Sem GameEventSourcingT IO ()
playCommandC :: Members '[MVar.MVarConcurrency, GameEventSourcing, State TableState] effects => [ConcurrentPlayer] -> GameCommand -> Sem effects ()
playCommandC concurrentPlayers gameCommand =
  do events <- gameCommandEventsM gameCommand
     maybeWinner <- gameOverM -- FIXME: should be GameOver in events
     case maybeWinner of
       Just _ -> return ()
       Nothing ->
         -- FIXME: do playEvent in GameEventSourcingT?
         do commandss <- mapM (\ event -> playEventC concurrentPlayers event) events
            mapM_ (playCommandC concurrentPlayers) (concat commandss)
            return ()

-- playGameC :: 
playGameC :: Members '[MVar.MVarConcurrency, GameEventSourcing, State TableState] effects =>
   [ConcurrentPlayer] -> [Card] -> Sem effects ()
playGameC concurrentPlayers cards =
  do  let shuffledCards = cards
      -- FIXME: split here
      let players = map (\ (ConcurrentPlayer player _) -> player)  concurrentPlayers
          hands = Map.fromList (zip players (map Set.fromList (distribute (length players) shuffledCards)))
      playCommandC concurrentPlayers (DealHands hands)

stateTeletypeToIO :: IORef s -> Sem '[State s, Teletype] a -> Sem '[Embed IO] a
stateTeletypeToIO state action = teletypeToIO (State.runStateIORef state (raise2Under action))

stateTeletypeConcurrentPlayer :: Player -> Strategy '[State (PlayerState player), Teletype] -> IORef (PlayerState player) -> IO ConcurrentPlayer
stateTeletypeConcurrentPlayer player strategy state =
  concurrentPlayer player (\ player event -> stateTeletypeToIO state (strategy player event))

-- gameInteractiveC :: IO [GameEvent]
gameInteractiveC =
  do let player1 = Player "1" "Mike"
         strategy1 :: Strategy '[State (PlayerState "Mike"), Teletype]
         strategy1 = interactiveStrategy @"Mike"
         player2 = Player "2" "Peter"
         strategy2 :: Strategy '[State (PlayerState "Peter"), Teletype]
         strategy2 = interactiveStrategy @"Peter"
         player3 = Player "3" "Nicole"
         strategy3 :: Strategy '[State (PlayerState "Nicole"), Teletype]
         strategy3 = interactiveStrategy @"Nicole"
         player4 = Player "4" "Annette"
         strategy4 :: Strategy '[State (PlayerState "Annette"), Teletype]
         strategy4 = interactiveStrategy @"Annette"
         players = [player1, player2, player3, player4]
     state1 <- IORef.newIORef emptyPlayerState
     state2 <- IORef.newIORef emptyPlayerState
     state3 <- IORef.newIORef emptyPlayerState
     state4 <- IORef.newIORef emptyPlayerState
     cplayer1 <- stateTeletypeConcurrentPlayer player1 strategy1 state1
     cplayer2 <- stateTeletypeConcurrentPlayer player2 strategy2 state2
     cplayer3 <- stateTeletypeConcurrentPlayer player3 strategy3 state3
     cplayer4 <- stateTeletypeConcurrentPlayer player4 strategy4 state4
     let concurrentPlayers = [cplayer1, cplayer2, cplayer3, cplayer4]
     let playGame = playGameC concurrentPlayers deck :: Sem '[MVar.MVarConcurrency, GameEventSourcing, State TableState] ()
     let playGameE = runEventSourcingState (MVar.mVarConcurrencyToIO (raise3Under @(Embed IO) playGame))
     events <- runM (State.evalState (emptyTableState players) (State.execState [] playGameE))
     return events     
