{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs, FlexibleContexts, TypeOperators, DataKinds, PolyKinds #-}
{-# LANGUAGE LambdaCase, BlockArguments, TypeApplications, ScopedTypeVariables, ConstraintKinds #-}
{-# OPTIONS_GHC -fplugin=Polysemy.Plugin #-}
module Sync where

import Debug.Trace (trace)

import qualified Control.Monad as Monad

import qualified Data.Map.Strict as Map
import Data.Map.Strict (Map, (!))

import qualified Data.Set as Set
import Data.Set (Set)

import Data.Foldable as Foldable

import Polysemy
import qualified Polysemy.State as State
import Polysemy.State (State)

import qualified Teletype
import Teletype (Teletype)

import qualified Data.IORef as IORef
import Data.IORef (IORef)

import Cards
import GameEvent
import Table
import Player
import EventSourcing
import TableEventSourcing
import Shuffle


-- synchroner Satz Spieler
type Players effects = Map Player (Strategy effects)

emptyPlayers :: Players effects
emptyPlayers = Map.empty

-- Spieler zu Spielersatz hinzufügen

addPlayer :: Players effects -> Player -> Strategy effects -> Players effects
addPlayer players player strategy =
  Map.insert player strategy players

mike = Player "1" "Mike"
bianca = Player "2" "Bianca"

type InteractivePlayerEffects player effects =
   (Member (State (PlayerState player)) effects, Member Teletype effects)

strategy1 :: InteractivePlayerEffects "Mike" effects => Strategy effects
strategy1 = interactiveStrategy @"Mike"
strategy2 :: InteractivePlayerEffects "Bianca" effects => Strategy effects
strategy2 = interactiveStrategy @"Bianca"
strategy3 :: InteractivePlayerEffects "Fredo" effects => Strategy effects
strategy3 = interactiveStrategy @"Fredo"
strategy4 :: InteractivePlayerEffects "Connie" effects => Strategy effects
strategy4 = interactiveStrategy @"Connie"

event :: GameEvent
event = undefined

both :: Sem '[State (PlayerState "Mike"), State (PlayerState "Bianca"), Teletype] ()
both =
  do commands1 <- strategy1 mike event
     commands2 <- strategy2 bianca event
     return ()


-- ein Event von den Spielern verarbeiten lassen
playEvent :: Players effects -> GameEvent -> Sem effects [GameCommand]
-- playEvent players gameEvent | trace ("playEvent " ++ show gameEvent) False = undefined
playEvent players gameEvent =
  Monad.foldM (\ gameCommands (player, playerStrategy) ->
                do gameCommands' <- playerStrategy player gameEvent
                   return (gameCommands ++ gameCommands'))
              []
              (Map.toList players)


-- Befehle ausführen bis zum bitteren Ende
playCommand :: TableEventSourcing effects => Players effects -> GameCommand -> Sem effects ()
playCommand players gameCommand | trace ("playCommand " ++ show gameCommand) False = undefined
playCommand players gameCommand =
  do events <- gameCommandEventsM gameCommand
     maybeWinner <- gameOverM
     case maybeWinner of 
       Just _ -> return ()
       Nothing ->
        do gameCommandss <- mapM (\ gameEvent -> playEvent players gameEvent) events
           let gameCommands = Monad.join gameCommandss
           mapM_ (playCommand players) gameCommands
           return ()

-- das Spiel spielen
playGame :: TableEventSourcing effects => Players effects -> [Card] -> Sem effects ()
playGame players shuffledCards = do
  let playerList = Map.keys players
      hands = Map.fromList (zip playerList (map Set.fromList (distribute (length playerList) shuffledCards)))
  playCommand players (DealHands hands)


type AlongEffects = '[EventSourcing GameEvent, State TableState,
                      State (PlayerState "Mike"), State (PlayerState "Peter"),
                      State (PlayerState "Nicole"), State (PlayerState "Annette")]

-- Spiel mit automatischen Spielern spielen
gameAlong :: IO [GameEvent]
gameAlong =
  do let player1 = Player "1" "Mike"
         strategy1 :: Strategy AlongEffects
         strategy1 = alongStrategy @"Mike"
         player2 = Player "2" "Peter"
         strategy2 :: Strategy AlongEffects
         strategy2 = alongStrategy @"Peter"
         player3 = Player "3" "Nicole"
         strategy3 :: Strategy AlongEffects
         strategy3 = alongStrategy @"Nicole"
         player4 = Player "4" "Annette"
         strategy4 :: Strategy AlongEffects
         strategy4 = alongStrategy @"Annette"
     let players1 = addPlayer emptyPlayers player1 strategy1
         players2 = addPlayer players1 player2 strategy2
         players3 = addPlayer players2 player3 strategy3
         players4 = addPlayer players3 player4 strategy4
     let playerNames = Map.keys players4
     shuffledDeck <- shuffle deck

     let serwt = runEventSourcingState (playGame players4 shuffledDeck)
     let srwt = State.execState [] serwt
     let rwt = State.evalState (emptyTableState playerNames) srwt
     let events =
           run (State.evalState emptyPlayerState (State.evalState emptyPlayerState (State.evalState emptyPlayerState (State.evalState emptyPlayerState rwt))))
     return events

type InteractiveEffects =
  '[ EventSourcing GameEvent,
     State TableState,
     State (PlayerState "Mike"),
     State (PlayerState "Peter"),
     State (PlayerState "Nicole"),
     State (PlayerState "Annette"),
     Teletype
   ]
gameInteractive :: IO ()
gameInteractive =
  do let player1 = Player "1" "Mike"
         strategy1 :: Strategy InteractiveEffects
         strategy1 = interactiveStrategy @"Mike"
         player2 = Player "2" "Peter"
         strategy2 :: Strategy InteractiveEffects
         strategy2 = interactiveStrategy @"Peter"
         player3 = Player "3" "Nicole"
         strategy3 :: Strategy InteractiveEffects
         strategy3 = interactiveStrategy @"Nicole"
         player4 = Player "4" "Annette"
         strategy4 :: Strategy InteractiveEffects
         strategy4 = interactiveStrategy @"Annette"
     let players1 = addPlayer emptyPlayers player1 strategy1
         players2 = addPlayer players1 player2 strategy2
         players3 = addPlayer players2 player3 strategy3
         players4 = addPlayer players3 player4 strategy4
     let playerNames = Map.keys players4
     shuffledDeck <- shuffle deck
     let serwt = runEventSourcingState (playGame players4 shuffledDeck)
     let srwt = State.execState [] serwt
     let rwt = State.evalState (emptyTableState playerNames) srwt
     _ <- Teletype.runTeletype (State.runState emptyPlayerState (State.runState emptyPlayerState (State.runState emptyPlayerState (State.runState emptyPlayerState rwt))))
     return ()
