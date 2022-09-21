{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE GADTs, FlexibleContexts, TypeOperators, DataKinds, PolyKinds #-}
{-# LANGUAGE LambdaCase, BlockArguments, TypeApplications, ScopedTypeVariables, ConstraintKinds #-}
{-# OPTIONS_GHC -fplugin=Polysemy.Plugin #-}

module Player where

import qualified Data.Set as Set
import Data.Set (Set)
import qualified Data.Map.Strict as Map
import Data.Map.Strict (Map, (!))

import Cards
import GameEvent

import Polysemy
import qualified Polysemy.State as State
import Polysemy.State (State)

import qualified Teletype
import Teletype (Teletype)

-- import Polysemy.Bundle

-- Spieler kennt das, was er sieht
data PlayerState player =
  PlayerState { playerStateHand  :: Hand,
                playerStateTrick :: Trick,
                playerStatePile :: [Card] }
  deriving Show

emptyPlayerState = PlayerState emptyHand [] []

-- Spielevent "sehen"
playerProcessEvent :: Player -> GameEvent -> PlayerState player -> PlayerState player
playerProcessEvent player (HandDealt player' hand) state
  | player == player' =
    PlayerState { playerStateHand = hand,
                  playerStateTrick = emptyTrick,
                  playerStatePile = [] }
  | otherwise = state
playerProcessEvent player (PlayerTurnChanged player') state = state
playerProcessEvent player (LegalCardPlayed player' card) state
  | player' == player =
    state { playerStateHand = removeCard card (playerStateHand state),
            playerStateTrick = addToTrick player' card (playerStateTrick state) }
  | otherwise =
    state { playerStateTrick = addToTrick player' card (playerStateTrick state) }
playerProcessEvent player (TrickTaken player' trick) state
  | player' == player =
    state { playerStateTrick = emptyTrick,
            playerStatePile = (cardsOfTrick trick) ++ (playerStatePile state) }
  | otherwise =
    state { playerStateTrick = emptyTrick }
playerProcessEvent player (IllegalCardAttempted player' card) state = state
playerProcessEvent player (GameEnded winner) state = state

-- monadische Version
playerProcessEventM ::
  Member (State (PlayerState player)) effects => Player -> GameEvent -> Sem effects ()
playerProcessEventM player event =
  do playerState <- State.get
     let playerState' = playerProcessEvent player event playerState
     State.put playerState'

type GameEventProcessor effects = GameEvent -> Sem effects [GameCommand]

-- ... macht aus Events Commands
type Strategy effects = Player -> GameEventProcessor effects

type Chooser player effects =
  Player -> PlayerState player -> Sem effects Card

-- Das hier ist schade, weil es bedeutet, daß PlayerState player bereits
-- in den Effekten des Chooser stehen muß.
-- Aber sonst wird es später schwierig, die entstehenden Strategien
-- in einer Monade zu kombinieren.
chooserStrategy ::
  Member (State (PlayerState player)) effects => Chooser player effects -> Strategy effects
chooserStrategy choose =
  \ player event ->
    do playerProcessEventM player event
       playerState <- State.get
       case event of
         HandDealt player' hand ->
           if (player == player') && (containsCard (Card Clubs Two) hand)
           then return [PlayCard player (Card Clubs Two)]
           else return []
         PlayerTurnChanged player' ->
           if player == player'
           then do card <- choose player playerState
                   return [PlayCard player card]
           else return []
         LegalCardPlayed player' card -> return []
         IllegalCardAttempted player' card -> return []
         TrickTaken player' trick -> return []
         GameEnded winner -> return []

-- Strategie für Roboterspieler
chooseAlong :: Chooser player effects
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

alongStrategy :: forall player effects . Member (State (PlayerState player)) effects => Strategy effects
alongStrategy player event = -- braucht Eta-Expansion
  chooserStrategy chooseAlong player event

-- newtype InteractiveEffect player monad a = InteractiveEffect (Bundle '[State (PlayerState player), Teletype] monad a)

-- interaktiver Spieler
chooseInteractive :: forall player effects . Member Teletype effects => Chooser player effects
chooseInteractive player playerState =
  do Teletype.writeTTY ("Your turn, player " ++ (playerName player))
     case playerStateTrick playerState of
       [] ->
         Teletype.writeTTY "You lead the next trick."
       trick ->
         Teletype.writeTTY ("Trick: " ++ prettyTrick trick)
     let hand = playerStateHand playerState
         handList = Set.elems hand
         ncards = Set.size hand
     Teletype.writeTTY ("Your hand:\n" ++ prettyCards handList)
     Teletype.writeTTY ("Pick a card (1-" ++ show ncards ++ ")")
     selected <- getNumber (1,ncards)
     return (handList !! (selected - 1))

prettyTrick trick =
  let prettyOne (player, card) = prettyCard card ++ " from " ++ prettyPlayer player
      pretty [] = ""
      pretty [one] = prettyOne one
      pretty (one:rest) = prettyOne one ++ ", " ++ pretty rest
  in pretty (reverse trick)

-- Spieleridentität drucken
prettyPlayer (Player id name) = name ++ "(" ++ id ++ ")"


-- |Eine Karte ausdrucken
prettyCard :: Card -> String
prettyCard c = show (rank c) ++ " of " ++ show (suit c)

-- |Liste von Karten ausdrucken
prettyCards :: [Card] -> String
prettyCards cards =
  let loop _ [] = ""
      loop n [card] = prettyOne n card
      loop n (card:cards) = prettyOne n card ++ "\n" ++ loop (n+1) cards
      prettyOne n card = "(" ++ (show n) ++ ") " ++ prettyCard card
  in loop 1 cards

-- Zahl einlesen
getNumber :: (Num a, Ord a, Read a, Show a, Member Teletype effects) => (a, a) -> Sem effects a
getNumber (lo, hi) = do
  s <- Teletype.readTTY
  let input = read s
  if lo <= input && input <= hi
  then return input
  else
    do Teletype.writeTTY ("Input must be between " ++ (show lo) ++ " and " ++ (show hi) ++ ". Try again")
       getNumber (lo, hi)

interactiveStrategy :: forall player effects . (Member Teletype effects, Member (State (PlayerState player)) effects) => Strategy effects
interactiveStrategy player event =
  chooserStrategy chooseInteractive player event

