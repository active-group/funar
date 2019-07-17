{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
module Player where

import qualified Control.Monad.State.Lazy as State
import Control.Monad.State.Lazy (State, StateT, MonadState)
import Control.Monad.Trans.Class

import qualified Data.Set as Set
import Data.Set (Set)
import qualified Data.Map.Strict as Map
import Data.Map.Strict (Map, (!))

import Cards
import Game
import RWT

-- Spielerstrategie
data Strategy monadT =
  Strategy (MonadTrans monadT => forall monad . Monad monad => Player -> EventProcessor (monadT monad) GameEvent GameCommand)

-- ... macht aus Events Commands
type EventProcessor monad event command = event -> monad [command]

-- Spieler kennt das, was er sieht
data PlayerState =
  PlayerState { playerStateHand  :: Hand,
                playerStateTrick :: Trick,
                playerStateStack :: [Card] }
  deriving Show

-- Spieler, der nur spielt, wenn er dran ist
chooserStrategy ::
  MonadTrans monadT =>
    (forall monad . (Monad monad, MonadState PlayerState (monadT monad)) => Player -> PlayerState -> (monadT monad) Card)
      -> (forall monad . (Monad monad, MonadState PlayerState (monadT monad)) => Player -> EventProcessor (monadT monad) GameEvent GameCommand)
chooserStrategy choose =
  \ player event ->
    do playerProcessGameEventM player event
       playerState <- State.get
       case event of
         HandDealt player' hand ->
           if (player == player') && (containsCard twoOfClubs hand)
           then return [PlayCard player twoOfClubs]
           else return []
         PlayerTurnChanged player' ->
           if player == player'
           then do card <- choose player playerState
                   return [PlayCard player card]
           else return []
         LegalCardPlayed player' card -> return []
         IllegalCardPlayed player' card -> return []
         TrickTaken player' trick -> return []
         GameEnded winner -> return []

emptyPlayerState = PlayerState emptyHand [] []

-- Spielevent "sehen"
playerProcessGameEvent :: Player -> GameEvent -> PlayerState -> PlayerState
playerProcessGameEvent player (HandDealt player' hand) state
  | player == player' =
    PlayerState { playerStateHand = hand,
                  playerStateTrick = emptyTrick,
                  playerStateStack = [] }
  | otherwise = state
playerProcessGameEvent player (PlayerTurnChanged player') state = state
playerProcessGameEvent player (LegalCardPlayed player' card) state
  | player' == player =
    state { playerStateHand = removeCard card (playerStateHand state),
            playerStateTrick = addToTrick player' card (playerStateTrick state) }
  | otherwise =
    state { playerStateTrick = addToTrick player card (playerStateTrick state) }
playerProcessGameEvent player (TrickTaken player' trick) state
  | player' == player =
    state { playerStateTrick = emptyTrick,
            playerStateStack = (cardsOfTrick trick) ++ (playerStateStack state) }
  | otherwise =
    state { playerStateTrick = emptyTrick }
playerProcessGameEvent player (IllegalCardPlayed player' card) state = state
playerProcessGameEvent player (GameEnded winner) state = state

-- monadische Version
playerProcessGameEventM :: MonadState PlayerState monad => Player -> GameEvent -> monad ()
playerProcessGameEventM player event =
  do playerState <- State.get
     let playerState' = playerProcessGameEvent player event playerState
     State.put playerState'

-- Stratgie für Roboterspieler
chooseAlong :: Monad m => Player -> PlayerState -> m Card
chooseAlong _ playerState =
  case playerStateTrick playerState of
    [] -> return (Set.findMin (playerStateHand playerState))       -- leine erste Karte
    trick ->
      let hand = playerStateHand playerState
          (_, firstCard) = last trick
          firstSuit = suit firstCard
          followingCardsOnHand = Set.filter ((== firstSuit) . suit) hand
      in case Set.lookupMin followingCardsOnHand of
           Nothing ->
             return (Set.findMax hand) -- wir haben nix passendes, nimm große Karte
           Just card ->
             return card           -- sonst kleine passende

playAlong player event =
  chooserStrategy chooseAlong player event

alongStrategy :: Strategy (StateT PlayerState)
alongStrategy = Strategy playAlong

-- interaktiver Spieler
chooseInteractive :: Monad monad => Player -> PlayerState -> RWT state monad Card
chooseInteractive player playerState =
  do RWT.putString ("Your turn, player " ++ (playerName player))
     case playerStateTrick playerState of
       [] ->
         RWT.putString "You lead the next trick."
       trick ->
         RWT.putString ("Cards on table: " ++ show (reverse trick))
     let hand = playerStateHand playerState
         handList = Set.elems hand
         ncards = Set.size hand
     RWT.putString ("Your hand: " ++ prettyCards handList)
     RWT.putString ("Pick a card (1-" ++ show ncards ++ ")")
     selected <- RWT.getNumber (1,ncards)
     return (handList !! (selected - 1))

playInteractive player event =
  chooserStrategy chooseInteractive player event

interactiveStrategy = Strategy playInteractive
