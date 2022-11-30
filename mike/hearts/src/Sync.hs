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

import Cards
import GameEvent
import Player
import Shuffle
import TableIO

type IOStrategy = GameEvent -> IO [GameCommand]

gameLoop :: (Player, IOStrategy) 
         -> (Player, IOStrategy)
         -> (Player, IOStrategy)
         -> (Player, IOStrategy) 
              -> IO (Maybe Player)
gameLoop (player1, playerIO1) (player2, playerIO2) (player3, playerIO3) (player4, playerIO4) =
 do let players = [player1, player2, player3, player4]
    shuffledDeck <- shuffle deck
    game <- tableIO players
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
    let hands = Map.fromList (zip players (map makeHand (distribute (length players) shuffledDeck)))
    loop [DealHands hands]

gameAlongIO :: IO (Maybe Player)
gameAlongIO =
  do
    let player1 = Player "Mike"
        strategy1 = alongStrategy player1
        player2 = Player "Peter"
        strategy2 = alongStrategy player2
        player3 = Player "Nicole"
        strategy3 = alongStrategy player3
        player4 = Player "Annette"
        strategy4 = alongStrategy player4
    playerIO1 <- statePlayerIO player1 strategy1
    playerIO2 <- statePlayerIO player2 strategy2
    playerIO3 <- statePlayerIO player3 strategy3
    playerIO4 <- statePlayerIO player4 strategy4
    gameLoop (player1, playerIO1) (player2, playerIO2) (player3, playerIO3) (player4, playerIO4)

gameInteractiveIO :: IO (Maybe Player)
gameInteractiveIO =
  do
    let player1 = Player "Mike"
        strategy1 = interactiveStrategy player1
        player2 = Player "Peter"
        strategy2 = interactiveStrategy player2
        player3 = Player "Nicole"
        strategy3 = interactiveStrategy player3
        player4 = Player "Annette"
        strategy4 = interactiveStrategy player4
    playerIO1 <- stateTtyPlayerIO player1 strategy1
    playerIO2 <- stateTtyPlayerIO player2 strategy2
    playerIO3 <- stateTtyPlayerIO player3 strategy3
    playerIO4 <- stateTtyPlayerIO player4 strategy4
    gameLoop (player1, playerIO1) (player2, playerIO2) (player3, playerIO3) (player4, playerIO4)

