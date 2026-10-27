{-# LANGUAGE FlexibleInstances #-}

module StatePlayer where

import GameEvent (GameCommand (..), GameEvent (..))
import Cards
import ChooserPlayer

import Data.Foldable (minimumBy, maximumBy)
import Free
import Data.IORef (IORef)
import qualified Data.IORef as IORef

data StatePlayer' r
  = GetEvent (GameEvent -> r)
  | RecordCommand GameCommand (() -> r)
  | GetPlayer (Player -> r)
  | GetHand (Hand -> r)
  | GetTrick (Trick -> r)

instance Functor StatePlayer' where
  fmap f (GetEvent cont) = GetEvent (\event -> f (cont event))
  fmap f (RecordCommand command cont) = RecordCommand command (\() -> f (cont ()))
  fmap f (GetPlayer cont) = GetPlayer (\player -> f (cont player))
  fmap f (GetHand cont) = GetHand (\hand -> f (cont hand))
  fmap f (GetTrick cont) = GetTrick (\trick -> f (cont trick))

type StatePlayer a = Free StatePlayer' a

instance Show a => Show (StatePlayer a) where
  show (Return result) = show result
  show (Impure (GetEvent _)) = "GetEvent"
  show (Impure (RecordCommand command _)) = "RecordCommand" ++ show command
  show (Impure (GetPlayer _)) = "GetPlayer"
  show (Impure (GetHand _)) = "GetHand"
  show (Impure (GetTrick _)) = "GetTrick"

getEventM :: StatePlayer GameEvent
getEventM = Impure (GetEvent Return)

recordCommandM :: GameCommand -> StatePlayer ()
recordCommandM command = Impure (RecordCommand command Return)

getPlayerM :: StatePlayer Player
getPlayerM = Impure (GetPlayer Return)

getHandM :: StatePlayer Hand
getHandM = Impure (GetHand Return)

getTrickM :: StatePlayer Trick
getTrickM = Impure (GetTrick Return)

-- Spieler kennt das, was er sieht (und sich selbst)
data PlayerState = PlayerState
  { playerStateHand :: Hand,
    playerStateTrick :: Trick,
    playerStatePile :: Pile,
    playerStatePlayer :: Player
  }
  deriving (Show)

makeEmptyPlayerStateFor = PlayerState emptyHand emptyTrick emptyPile

-- Spielevent "sehen"
playerProcessEvent :: GameEvent -> PlayerState -> PlayerState
playerProcessEvent (HandDealt player' hand) state =
  if playerStatePlayer state == player'
    then
      state
        { playerStateHand = hand,
          playerStateTrick = emptyTrick,
          playerStatePile = emptyPile
        }
    else state
playerProcessEvent (PlayerTurnChanged player') state = state
playerProcessEvent (LegalCardPlayed player' card) state =
  if playerStatePlayer state == player'
    then
      state
        { playerStateHand = removeCard card (playerStateHand state),
          playerStateTrick = addToTrick player' card (playerStateTrick state)
        }
    else state {playerStateTrick = addToTrick player' card (playerStateTrick state)}
playerProcessEvent (TrickTaken player' trick) state =
  if playerStatePlayer state == player'
    then
      state
        { playerStateTrick = emptyTrick,
          playerStatePile = pileAddTrick (playerStatePile state) trick
        }
    else state {playerStateTrick = emptyTrick}
playerProcessEvent (InvalidCardAttempted player' card) state = state
playerProcessEvent (GameEnded winner) state = state

runStatePlayer ::
  StatePlayer a ->
  (PlayerState, [GameCommand]) ->
  (Either (GameEvent -> (PlayerState, StatePlayer a)) a, [GameCommand])
runStatePlayer (Return result) (state, commands) =
  (Right result, reverse commands)
runStatePlayer (Impure (GetEvent cont)) (state, commands) =
  (Left (\event -> (playerProcessEvent event state, cont event)), reverse commands)
runStatePlayer (Impure (RecordCommand command cont)) (state, commands) =
  runStatePlayer (cont ()) (state, command : commands)
runStatePlayer (Impure (GetPlayer cont)) s@(state, _) =
  runStatePlayer (cont (playerStatePlayer state)) s
runStatePlayer (Impure (GetHand cont)) s@(state, _) =
  runStatePlayer (cont (playerStateHand state)) s
runStatePlayer (Impure (GetTrick cont)) s@(state, _) =
  runStatePlayer (cont (playerStateTrick state)) s

statePlayerIO :: Player -> StatePlayer a -> IO (GameEvent -> IO [GameCommand])
statePlayerIO player playerM =
  do
    let emptyPlayerState = makeEmptyPlayerStateFor player
        (step0, commands0) = runStatePlayer playerM (emptyPlayerState, [])
    ref <- IORef.newIORef (step0, emptyPlayerState, commands0)
    let processEvent event =
          do
            (step, state, commands) <- IORef.readIORef ref
            case step of
              Left cont ->
                do
                  let (state', playerM') = cont event
                  let (step', commands') = runStatePlayer playerM' (state', [])
                  IORef.writeIORef ref (step', state', [])
                  return (commands ++ commands')
              Right _result -> return commands
    return processEvent

chooserPlayerOps = ChooserPlayerOps getEventM recordCommandM

alongStrategy :: Player -> StatePlayer ()
alongStrategy player = chooserStrategy chooserPlayerOps player chooseAlong

-- Strategie für Roboterspieler
chooseAlong :: StatePlayer Card
chooseAlong =
  do
    hand <- getHandM
    let cards = handCards hand
    trick <- getTrickM
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
