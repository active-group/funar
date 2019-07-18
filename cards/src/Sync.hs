module Sync where

import Debug.Trace (trace)

import qualified Data.Map.Strict as Map
import Data.Map.Strict (Map)
import qualified Data.Set as Set
import Data.Set (Set)

import Data.Foldable as Foldable

import qualified Control.Monad as Monad

import Control.Monad.Identity (Identity, IdentityT)
import qualified Control.Monad.Identity as Identity

import qualified Control.Monad.State.Lazy as State
import Control.Monad.State.Lazy (State, StateT, MonadState)
import Control.Monad.Trans.Class

import qualified Control.Monad.Writer (WriterT)
import Control.Monad.Writer as Writer

import qualified Data.IORef as IORef
import Data.IORef (IORef)

import RWT

import Cards
import Game
import Player
import EventSourcing
import Shuffle

-- synchroner Satz Spieler
type Players monad = Map Player (EventProcessor monad GameEvent GameCommand)

emptyPlayers :: Players monad
emptyPlayers = Map.empty

-- Spieler zu Spielersatz hinzufügen
addPlayer :: (MonadTrans monadT, Monad monad) => Players monad -> Player -> Strategy monadT -> Players (monadT monad)
addPlayer players player (Strategy play) =
  let liftProcessor processor = \ event -> lift (processor event)
  in Map.insert player (play player) (Map.map liftProcessor players)

-- ein Event von den Spielern verarbeiten lassen
playEvent :: Monad monad => Players monad -> GameEvent -> monad [GameCommand]
-- playEvent players gameEvent | trace ("playEvent " ++ show gameEvent) False = undefined
playEvent players gameEvent =
  Monad.foldM (\ gameCommands playerProcessor ->
                do gameCommands' <- playerProcessor gameEvent
                   return (gameCommands ++ gameCommands'))
              []
              players

-- Befehle ausführen bis zum bitteren Ende
playCommand :: Monad monad => Players monad -> GameCommand -> GameEventSourcingT monad ()
-- playCommand players gameCommand | trace ("playCommand " ++ show gameCommand) False = undefined
playCommand players gameCommand =
  do events <- gameCommandEventsM gameCommand
     gameOver <- gameOverM
     if gameOver
     then return ()
     else
       do gameCommandss <- mapM (\ gameEvent -> lift (lift (playEvent players gameEvent))) events
          let gameCommands = Monad.join gameCommandss
          mapM_ (playCommand players) gameCommands
          return ()

-- das Spiel spielen
playGame :: Monad monad => Players monad -> [Card] -> GameEventSourcingT monad ()
playGame players shuffledCards = do
  let playerList = Map.keys players
      hands = Map.fromList (zip playerList (map Set.fromList (distribute (length playerList) shuffledCards)))
  playCommand players (DealHands hands)

-- Spiel mit automatischen Spielern spielen
gameAlong :: IO [GameEvent]
gameAlong =
  do let player1 = Player "1" "Mike"
         player2 = Player "2" "Peter"
         player3 = Player "3" "Nicole"
         player4 = Player "4" "Annette"
     let players1 = addPlayer (emptyPlayers :: Players Identity) player1 alongStrategy
         players2 = addPlayer players1 player2 alongStrategy
         players3 = addPlayer players2 player3 alongStrategy
         players4 = addPlayer players3 player4 alongStrategy
     let playerNames = Map.keys players4
     shuffledDeck <- shuffle deck
     let rwt = execWriterT (State.evalStateT (playGame players4 shuffledDeck) (emptyGameState playerNames))
     let ((((events, state4), state3), state2), state1) = Identity.runIdentity (State.runStateT (State.runStateT (State.runStateT (State.runStateT rwt emptyPlayerState) emptyPlayerState) emptyPlayerState) emptyPlayerState)
     return events

-- Spiel mit interaktiven Spielern spielen
gameInteractive :: IO ()
gameInteractive =
  do let player1 = Player "1" "Mike"
         player2 = Player "2" "Peter"
         player3 = Player "3" "Nicole"
         player4 = Player "4" "Annette"
     let players1 = addPlayer (emptyPlayers :: Players Identity) player1 interactiveStrategy
         players2 = addPlayer players1 player2 interactiveStrategy
         players3 = addPlayer players2 player3 interactiveStrategy
         players4 = addPlayer players3 player4 interactiveStrategy
     state1 <- IORef.newIORef emptyPlayerState
     state2 <- IORef.newIORef emptyPlayerState
     state3 <- IORef.newIORef emptyPlayerState
     state4 <- IORef.newIORef emptyPlayerState
     let playerNames = Map.keys players4
     shuffledDeck <- shuffle deck
     let rwt = runWriterT (State.evalStateT (playGame players4 shuffledDeck) (emptyGameState playerNames))
     ((), events) <- runRWTIO (runRWTIO (runRWTIO (runRWTIO (return . Identity.runIdentity) state1) state2) state3) state4 rwt
     return ()

