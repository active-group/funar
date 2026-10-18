{-# LANGUAGE FlexibleInstances #-}

module StatePlayer where

import GameEvent (GameCommand (..), GameEvent (..))
import Player
import Cards

import Data.Foldable (minimumBy, maximumBy)
import Free
import Data.IORef (IORef)
import qualified Data.IORef as IORef

data StatePlayer' r
  = GetEvent (GameEvent -> r)
  | RecordCommand GameCommand (() -> r)
  | GetPlayerState (PlayerState -> r)

instance Functor StatePlayer' where
  fmap f (GetEvent cont) = GetEvent (\event -> f (cont event))
  fmap f (RecordCommand command cont) = RecordCommand command (\() -> f (cont ()))
  fmap f (GetPlayerState cont) = GetPlayerState (\state -> f (cont state))

type StatePlayer a = Free StatePlayer' a

instance Show a => Show (StatePlayer a) where
  show (Return result) = show result
  show (Impure (GetEvent _)) = "GetEvent"
  show (Impure (RecordCommand command _)) = "RecordCommand" ++ show command
  show (Impure (GetPlayerState _)) = "GetPlayerState"

getEventM :: StatePlayer GameEvent
getEventM = Impure (GetEvent Return)

recordCommandM :: GameCommand -> StatePlayer ()
recordCommandM command = Impure (RecordCommand command Return)

getPlayerStateM :: StatePlayer PlayerState
getPlayerStateM = Impure (GetPlayerState Return)

runStatePlayer ::
  StatePlayer a ->
  PlayerState ->
  [GameCommand] ->
  ([GameCommand], Either (GameEvent -> (PlayerState, StatePlayer a)) a)
runStatePlayer (Return result) state commands =
  (reverse commands, Right result)
runStatePlayer (Impure (GetEvent cont)) state commands =
  (reverse commands, Left (\event -> (playerProcessEvent event state, cont event)))
runStatePlayer (Impure (RecordCommand command cont)) state commands =
  runStatePlayer (cont ()) state (command : commands)
runStatePlayer (Impure (GetPlayerState cont)) state commands =
  runStatePlayer (cont state) state commands

statePlayerIO :: Player -> StatePlayer a -> IO (GameEvent -> IO [GameCommand])
statePlayerIO player playerM =
  do
    let emptyPlayerState = makeEmptyPlayerStateFor player
        (commands0, step0) = runStatePlayer playerM emptyPlayerState []
    ref <- IORef.newIORef (emptyPlayerState, commands0, step0)
    let processEvent event =
          do
            (state, commands, step) <- IORef.readIORef ref
            case step of
              Left cont ->
                do
                  let (state', playerM') = cont event
                  let (commands', step') = runStatePlayer playerM' state' []
                  IORef.writeIORef ref (state', [], step')
                  return (commands ++ commands')
              Right _result -> return commands
    return processEvent

statePlayerOps = StatePlayerOps getEventM recordCommandM getPlayerStateM

alongStrategy :: Player -> StatePlayer ()
alongStrategy player = chooserStrategy statePlayerOps player chooseAlong

-- Strategie für Roboterspieler
chooseAlong :: StatePlayer Card
chooseAlong =
  do
    playerState <- getPlayerStateM
    let cards = handCards (playerStateHand playerState)
    let trick = playerStateTrick playerState
    -- kleine erste Karte
    if trickEmpty trick
      then return (minimumBy compareCards cards)
      else
        let firstCard = leadingCardOfTrick trick
            firstSuit = suit firstCard
         in case filter (\card -> suit card == firstSuit) cards of
              [] ->
                return (maximumBy compareCards cards) -- wir haben nix passendes, nimm große Karte
              matchingCards ->
                return (minimumBy compareCards matchingCards) -- sonst kleine passende

compareCards card1 card2 = compare (rank card1) (rank card2)
