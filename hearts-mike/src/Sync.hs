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
import Polysemy.Cont

import qualified Teletype
import Teletype (Teletype)

import qualified Data.IORef as IORef
import Data.IORef (IORef)

import Cards
import GameEvent
import Table
import Player
import Game
import Shuffle
import GameIO
import PlayerIO

-- synchroner Satz Spieler
type Players effects = Map Player (Strategy effects)

emptyPlayers :: Players effects
emptyPlayers = Map.empty

-- Spieler zu Spielersatz hinzufügen

addPlayer :: Players effects -> Player -> Strategy effects -> Players effects
addPlayer players player strategy =
  Map.insert player strategy players

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

runPlayers :: Players effects -> GameEvent -> Sem effects [GameCommand]
runPlayers players event =
  do Monad.foldM
        (\commands (player, playerStrategy) ->
            do commands' <- playerStrategy player event
               return (commands ++ commands'))
        []
        (Map.toList players)

runGame :: GameStepEffects effects => Players effects -> [Card] -> Sem effects Player
runGame players shuffledCards =
  do let playerIds = Map.keys players
     processCommand <- gameSem playerIds
     let hands = Map.fromList (zip playerIds (map Set.fromList (distribute (length players) shuffledCards)))
     let loop events =
            do commands <- Monad.foldM (\commands event -> 
                                         do commands' <- runPlayers players event
                                            return (commands ++ commands'))
                                       []
                                       events
               events <- Monad.foldM (\events command ->
                                        do events' <- processCommand command 
                                           return (events ++ events'))
                                     []
                                     commands
               case eventsWinner events of
                Just winner -> return winner
                Nothing -> loop events
     events <- processCommand (DealHands hands)
     loop events

alongPlayers ::
  Members
    '[ State (PlayerState "Mike"),
       State (PlayerState "Peter"),
       State (PlayerState "Nicole"),
       State (PlayerState "Annette")
     ]
    effects =>
  Players effects
alongPlayers =
  let player1 = Player "1" "Mike"
      strategy1 = alongStrategy @"Mike"
      player2 = Player "2" "Peter"
      strategy2 = alongStrategy @"Peter"
      player3 = Player "3" "Nicole"
      strategy3 = alongStrategy @"Nicole"
      player4 = Player "4" "Annette"
      strategy4 = alongStrategy @"Annette"
      players1 = addPlayer emptyPlayers player1 strategy1
      players2 = addPlayer players1 player2 strategy2
      players3 = addPlayer players2 player3 strategy3
      players = addPlayer players3 player4 strategy4
   in players


-- FIXME: can probably go
runGameStepM :: Member (State TableState) effects =>
  Game a -> Sem effects (GameStep a)
runGameStepM game =
  do tableState <- State.get
     let (step, tableState') = runGameStep game tableState
     State.put tableState'
     return step

type AlongEffects = '[State [GameCommand], State GameStepState,
                      State (PlayerState "Mike"), State (PlayerState "Peter"),
                      State (PlayerState "Nicole"), State (PlayerState "Annette")]

-- Spiel mit automatischen Spielern spielen
gameAlong :: IO Player
gameAlong =
  do let players = alongPlayers @AlongEffects
     let playerIdentities = Map.keys players
     shuffledDeck <- shuffle deck
     let game = runGame players shuffledDeck
     let s1 = State.evalState [] game
     let s2 = State.evalState initialGameStepState s1
     let p1 = State.evalState emptyPlayerState s2
     let p2 = State.evalState emptyPlayerState p1
     let p3 = State.evalState emptyPlayerState p2
     let p4 = State.evalState emptyPlayerState p3
     return (run p4)

type PlayerEffects =
  '[ State (PlayerState "Mike"),
     State (PlayerState "Peter"),
     State (PlayerState "Nicole"),
     State (PlayerState "Annette")
   ]

gameAlongIO :: IO (Maybe Player)
gameAlongIO =
  do
    let player1 = Player "1" "Mike"
        strategy1 :: Strategy '[State (PlayerState "Mike")]
        strategy1 = alongStrategy
        player2 = Player "2" "Peter"
        strategy2 :: Strategy '[State (PlayerState "Peter")]
        strategy2 = alongStrategy
        player3 = Player "3" "Nicole"
        strategy3 :: Strategy '[State (PlayerState "Nicole")]
        strategy3 = alongStrategy
        player4 = Player "4" "Annette"
        strategy4 :: Strategy '[State (PlayerState "Annette")]
        strategy4 = alongStrategy
    let players = [player1, player2, player3, player4]
    shuffledDeck <- shuffle deck
    game <- gameIO players
    playerIO1 <- playerIO player1 strategy1
    playerIO2 <- playerIO player2 strategy2
    playerIO3 <- playerIO player3 strategy3
    playerIO4 <- playerIO player4 strategy4
    let loop :: [GameCommand] -> IO (Maybe Player)
        loop commands | trace ("loop " ++ (show commands)) False = undefined
        loop [] = return Nothing
        loop commands =
          do events <- fmap concat (mapM game commands)
             putStrLn ("events: " ++ show events)
             case eventsWinner events of
               Just winner ->
                 return (Just winner)
               Nothing -> 
                 do commands <-
                      Monad.foldM (\ commands event ->
                                      do commands1 <- playerIO1 event
                                         commands2 <- playerIO2 event 
                                         commands3 <- playerIO3 event 
                                         commands4 <- playerIO4 event
                                         let commands' = commands1 ++ commands2 ++ commands3 ++ commands4
                                         return (commands ++ commands'))
                      []
                      events
                    loop commands
    let hands = Map.fromList (zip players (map Set.fromList (distribute (length players) shuffledDeck)))
    loop [DealHands hands]

type InteractiveEffects =
  '[ State [GameCommand],
     State GameStepState,
     State (PlayerState "Mike"),
     State (PlayerState "Peter"),
     State (PlayerState "Nicole"),
     State (PlayerState "Annette"),
     Teletype
   ]
gameInteractive :: IO Player
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
         players = addPlayer players3 player4 strategy4
     let playerIdentities = Map.keys players
     shuffledDeck <- shuffle deck
     let game = runGame players shuffledDeck
     let s1 = State.evalState [] game
     let s2 = State.evalState initialGameStepState s1
     let p1 = State.evalState emptyPlayerState s2
     let p2 = State.evalState emptyPlayerState p1
     let p3 = State.evalState emptyPlayerState p2
     let p4 = State.evalState emptyPlayerState p3
     Teletype.runTeletype p4
