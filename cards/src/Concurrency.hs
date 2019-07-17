{-# LANGUAGE RankNTypes #-}
module Concurrency where

import qualified Data.Map.Strict as Map
import Data.Map.Strict (Map)
import qualified Data.Set as Set
import Data.Set (Set)

import qualified Data.IORef as IORef
import Data.IORef (IORef)

import Control.Concurrent
import Control.Monad.Trans.Class

import Control.Monad.Identity (Identity, IdentityT)
import qualified Control.Monad.Identity as Identity

import qualified Control.Monad.State.Lazy as State
import Control.Monad.State.Lazy (State, StateT, MonadState)
import Control.Monad.Trans.Class

import qualified Control.Monad.Writer (WriterT)
import Control.Monad.Writer as Writer

import Cards
import Game
import Player
import EventSourcing
import Shuffle
import RWT

data ConcurrentPlayer = ConcurrentPlayer Player (MVar (GameEvent, MVar [GameCommand]))

concurrentPlayer :: MonadTrans monadT => Player -> Strategy monadT -> (forall a . monadT IO a -> IO a) -> IO ConcurrentPlayer 
concurrentPlayer player (Strategy play) toIO =
  do events <- newEmptyMVar
     let recur =
           do  (event, commandsChannel) <- takeMVar events
               commands <- toIO (play player event)
               putMVar commandsChannel commands
               recur
     threadId <- forkIO recur
     return (ConcurrentPlayer player events)

playEventC :: [ConcurrentPlayer] -> GameEvent -> IO [GameCommand]
playEventC concurrentPlayers event =
  do commandsChannels <- mapM (\ (ConcurrentPlayer player events) ->
                                 do commandsChannel <- newEmptyMVar
                                    putMVar events (event, commandsChannel)
                                    return commandsChannel)
                              concurrentPlayers
     commandss <- mapM takeMVar commandsChannels
     return (concat commandss)

playCommandC :: [ConcurrentPlayer] -> GameCommand -> GameEventSourcingT IO ()
playCommandC concurrentPlayers gameCommand =
  do events <- gameCommandEventsM gameCommand
     gameOver <- gameOverM -- FIXME: should be GameOver in events
     if gameOver
     then return ()
     else
       -- FIXME: do playEvent in GameEventSourcingT?
       do commandss <- mapM (\ event -> lift (lift (playEventC concurrentPlayers event))) events
          mapM_ (playCommandC concurrentPlayers) (concat commandss)
          return ()

playGameC :: [ConcurrentPlayer] -> [Card] -> GameEventSourcingT IO ()
playGameC concurrentPlayers cards =
  do  let shuffledCards = cards
      -- FIXME: split here
      let players = map (\ (ConcurrentPlayer player _) -> player)  concurrentPlayers
          hands = Map.fromList (zip players (map Set.fromList (distribute (length players) shuffledCards)))
      playCommandC concurrentPlayers (DealHands hands)

gameInteractiveC :: IO [GameEvent]
gameInteractiveC =
  do let player1 = Player "1" "Mike"
         player2 = Player "2" "Peter"
         player3 = Player "3" "Nicole"
         player4 = Player "4" "Annette"
     state1 <- IORef.newIORef emptyPlayerState
     state2 <- IORef.newIORef emptyPlayerState
     state3 <- IORef.newIORef emptyPlayerState
     state4 <- IORef.newIORef emptyPlayerState
     let players = [player1, player2, player3, player4]
         states = [state1, state2, state3, state4]
     let playerNames = map playerName players
     concurrentPlayers <- sequence (zipWith (\ player state ->
                                               concurrentPlayer player interactiveStrategy (runRWTIO id state))
                                            players states)
     execWriterT (State.evalStateT (playGameC concurrentPlayers deck) (emptyGameState players))

