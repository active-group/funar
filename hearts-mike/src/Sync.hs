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
import Table ( TableState, emptyTableState )
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

interpretPlayers :: Member (State [GameCommand]) effects => 
                      Players effects -> Sem (GameStep ': effects) a -> Sem effects a
interpretPlayers players = interpret $ \case
  BroadcastEvent event ->
    Monad.mapM_ (\ (player, playerStrategy) ->
                  do commands <- State.get
                     commands' <- playerStrategy player event
                     State.put (commands ++ commands'))
                (Map.toList players)
  ReceiveCommand ->
    do gameCommands <- State.get 
       case gameCommands of
         [] -> return Nothing
         (gameCommand:gameCommands') ->
           do State.put gameCommands'
              return (Just gameCommand)    

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

type AlongEffects = '[State [GameCommand], State TableState,
                      State (PlayerState "Mike"), State (PlayerState "Peter"),
                      State (PlayerState "Nicole"), State (PlayerState "Annette")]

-- Spiel mit automatischen Spielern spielen
gameAlong :: IO (Maybe Player)
gameAlong =
  do let players = alongPlayers @AlongEffects
     let playerIdentities = Map.keys players
     shuffledDeck <- shuffle deck
     let game = Game.playGame playerIdentities shuffledDeck
     let gameWithPlayers = interpretPlayers players game
     let s1 = State.evalState [] gameWithPlayers
     let s2 = State.evalState (emptyTableState playerIdentities) s1
     let p1 = State.evalState emptyPlayerState s2
     let p2 = State.evalState emptyPlayerState p1
     let p3 = State.evalState emptyPlayerState p2
     let p4 = State.evalState emptyPlayerState p3
     return (run p4)

type GameStepEffects =
  '[ State TableState,
     GameStep,
     GameStepResultEffect
   ]


type PlayerEffects =
  '[ State (PlayerState "Mike"),
     State (PlayerState "Peter"),
     State (PlayerState "Nicole"),
     State (PlayerState "Annette")
   ]
-- wie gameAlong, aber Schritt für Schritt
gameAlongStep =
  do
    let players = alongPlayers @PlayerEffects
    let playersList = Map.toList players
    let playerIdentities = Map.keys players
    shuffledDeck <- shuffle deck    
    let game = Game.playGame playerIdentities shuffledDeck
    let game' = State.evalState (emptyTableState playerIdentities) game
    let gameOne = callCC (\ cont -> 
                              fmap GameDone (Game.playOne cont game'))
    let loop stepResult commands =
          case stepResult of
            GameDone mbPlayer -> return mbPlayer
            EventBroadcast event cont ->
              do commands' <- Monad.foldM
                               (\ commands (player, playerStrategy) ->
                                   do commands' <- playerStrategy player event
                                      return (commands ++ commands'))
                               []
                               playersList
                 let stepResult' = run (runContPure (cont ()))
                 loop stepResult' (commands ++ commands')
            WaitingForCommand cont ->
              case commands of
                [] -> return Nothing 
                (command:commands') ->
                  do let stepResult' = run (runContPure (cont command))
                     loop stepResult' commands'

    let p = loop (run (runContPure gameOne)) []
    let p1 = State.evalState emptyPlayerState p
    let p2 = State.evalState emptyPlayerState p1
    let p3 = State.evalState emptyPlayerState p2
    let p4 = State.evalState emptyPlayerState p3
    return (run p4)

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
        -- loop commands | trace ("loop " ++ (show commands)) False = undefined
        loop [] = return Nothing
        loop commands =
          do events <- fmap concat (mapM game commands)
             let isGameEnded (GameEnded _) = True 
                 isGameEnded _ = False
             case find isGameEnded events of
               Just (GameEnded winner) ->
                 return (Just winner)
               _ -> 
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
     State TableState,
     State (PlayerState "Mike"),
     State (PlayerState "Peter"),
     State (PlayerState "Nicole"),
     State (PlayerState "Annette"),
     Teletype
   ]
gameInteractive :: IO (Maybe Player)
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
     let game = Game.playGame playerIdentities shuffledDeck
     let gameWithPlayers = interpretPlayers players game
     let s1 = State.evalState [] gameWithPlayers
     let s2 = State.evalState (emptyTableState playerIdentities) s1
     let p1 = State.evalState emptyPlayerState s2
     let p2 = State.evalState emptyPlayerState p1
     let p3 = State.evalState emptyPlayerState p2
     let p4 = State.evalState emptyPlayerState p3
     Teletype.runTeletype p4
