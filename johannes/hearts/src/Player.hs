{-# LANGUAGE FlexibleInstances #-}

module Player where

import Cards
import Data.Foldable (minimumBy)
import Data.IORef (IORef)
import qualified Data.IORef as IORef
import Data.Map.Strict (Map, (!))
import qualified Data.Map.Strict as Map
import Data.Set (Set)
import qualified Data.Set as Set
import Debug.Trace (trace)
import Free
import GameEvent (GameCommand (..), GameEvent (..))

-- Spieler kennt das, was er sieht (und sich selbst)
data PlayerState = PlayerState
  { playerStateHand :: Hand,
    playerStateTrick :: Trick,
    playerStatePile :: [Card],
    playerStatePlayer :: Player
  }
  deriving (Show)

makeEmptyPlayerStateFor = PlayerState emptyHand emptyTrick []

-- Spielevent "sehen"
playerProcessEvent :: Player -> GameEvent -> PlayerState -> PlayerState
playerProcessEvent player (HandDealt player' hand) state =
  if player == player'
    then
      PlayerState
        { playerStateHand = hand,
          playerStateTrick = emptyTrick,
          playerStatePile = [],
          playerStatePlayer = player
        }
    else state
playerProcessEvent player (PlayerTurnChanged player') state = state
playerProcessEvent player (LegalCardPlayed player' card) state =
  if player' == player
    then
      state
        { playerStateHand = removeCard card (playerStateHand state),
          playerStateTrick = addToTrick player' card (playerStateTrick state)
        }
    else state {playerStateTrick = addToTrick player' card (playerStateTrick state)}
playerProcessEvent player (TrickTaken player' trick) state =
  if player' == player
    then
      state
        { playerStateTrick = emptyTrick,
          playerStatePile = (cardsOfTrick trick) ++ (playerStatePile state)
        }
    else state {playerStateTrick = emptyTrick}
playerProcessEvent player (IllegalCardAttempted player' card) state = state
playerProcessEvent player (GameEnded winner) state = state

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
  show (Pure result) = show result
  show (Impure (GetEvent _)) = "GetEvent"
  show (Impure (RecordCommand command _)) = "RecordCommand" ++ show command
  show (Impure (GetPlayerState _)) = "GetPlayerState"

class Monad m => StatePlayerMonad m where
  getEventM :: m GameEvent
  recordCommandM :: GameCommand -> m ()
  getPlayerStateM :: m PlayerState

instance StatePlayerMonad (Free StatePlayer') where
  getEventM = Impure (GetEvent Pure)
  recordCommandM command = Impure (RecordCommand command Pure)
  getPlayerStateM = Impure (GetPlayerState Pure)

runStatePlayer ::
  Player ->
  StatePlayer a ->
  PlayerState ->
  [GameCommand] ->
  ([GameCommand], Either (GameEvent -> (PlayerState, StatePlayer a)) a)
runStatePlayer player (Pure result) state commands =
  (reverse commands, Right result)
runStatePlayer player (Impure (GetEvent cont)) state commands =
  (reverse commands, Left (\event -> (playerProcessEvent player event state, cont event)))
runStatePlayer player (Impure (RecordCommand command cont)) state commands =
  runStatePlayer player (cont ()) state (command : commands)
runStatePlayer player (Impure (GetPlayerState cont)) state commands =
  runStatePlayer player (cont state) state commands

statePlayerIO :: Player -> StatePlayer a -> IO (GameEvent -> IO [GameCommand])
statePlayerIO player playerM =
  do
    let emptyPlayerState = makeEmptyPlayerStateFor player
        (commands0, step0) = runStatePlayer player playerM emptyPlayerState []
    ref <- IORef.newIORef (emptyPlayerState, commands0, step0)
    let processEvent event =
          do
            (state, commands, step) <- IORef.readIORef ref
            case step of
              Left cont ->
                do
                  let (state', playerM') = cont event
                  let (commands', step') = runStatePlayer player playerM' state' []
                  IORef.writeIORef ref (state', [], step')
                  return (commands ++ commands')
              Right _result -> return commands
    return processEvent

chooserStrategy ::
  StatePlayerMonad m =>
  Player ->
  m Card ->
  m ()
chooserStrategy player choose =
  do
    event <- getEventM
    state <- getPlayerStateM
    let hand = playerStateHand state
    case event of
      HandDealt player' hand ->
        if (player == player') && (containsCard (Card Clubs Two) hand)
          then recordCommandM (PlayCard player (Card Clubs Two))
          else return ()
      PlayerTurnChanged player' ->
        if player == player'
          then do
            card <- choose
            recordCommandM (PlayCard player card)
          else return ()
      LegalCardPlayed player' card -> return ()
      IllegalCardAttempted player' card -> return ()
      TrickTaken player' trick -> return ()
      GameEnded winner -> return ()
    chooserStrategy player choose

-- Strategie für Roboterspieler
chooseAlong :: StatePlayerMonad m => m Card
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
         in case filter ((== firstSuit) . suit) cards of
              [] ->
                -- TODO maximum?
                return (minimumBy compareCards cards) -- wir haben nix passendes, nimm große Karte
              matchingCards ->
                return (minimumBy compareCards matchingCards) -- sonst kleine passende

compareCards :: Card -> Card -> Ordering
compareCards card1 card2 = compare (rank card1) (rank card2)

alongStrategy :: StatePlayerMonad m => Player -> m ()
alongStrategy player = chooserStrategy player chooseAlong

data StateTtyPlayer' r
  = GetEventT (GameEvent -> r)
  | RecordCommandT GameCommand (() -> r)
  | GetPlayerStateT (PlayerState -> r)
  | ReadLineT (String -> r)
  | WriteLineT String (() -> r)

instance Functor StateTtyPlayer' where
  fmap f (GetEventT cont) = GetEventT (\event -> f (cont event))
  fmap f (RecordCommandT command cont) = RecordCommandT command (\() -> f (cont ()))
  fmap f (GetPlayerStateT cont) = GetPlayerStateT (\state -> f (cont state))
  fmap f (ReadLineT cont) = ReadLineT (\line -> f (cont line))
  fmap f (WriteLineT line cont) = WriteLineT line (\() -> f (cont ()))

type StateTtyPlayer a = Free StateTtyPlayer' a

instance StatePlayerMonad (Free StateTtyPlayer') where
  getEventM = Impure (GetEventT Pure)

  recordCommandM command = Impure (RecordCommandT command Pure)

  getPlayerStateM = Impure (GetPlayerStateT Pure)

class Monad m => TtyPlayerMonad m where
  readLineM :: m String
  writeLineM :: String -> m ()

instance TtyPlayerMonad (Free StateTtyPlayer') where
  readLineM = Impure (ReadLineT Pure)
  writeLineM line = Impure (WriteLineT line Pure)

-- interaktiver Spieler
chooseInteractive :: (StatePlayerMonad m, TtyPlayerMonad m) => m Card
chooseInteractive =
  do
    playerState <- getPlayerStateM
    writeLineM ("Your turn, player " ++ (playerName (playerStatePlayer playerState)))
    case trickToList (playerStateTrick playerState) of
      [] ->
        writeLineM "You lead the next trick."
      trick ->
        writeLineM ("Trick: " ++ prettyTrick trick)
    let hand = playerStateHand playerState
        cards = handCards hand
        ncards = length cards
    writeLineM ("Your hand:\n" ++ prettyCards cards)
    writeLineM ("Pick a card (1-" ++ show ncards ++ ")")
    selected <- getNumber (1, ncards)
    return (cards !! (selected - 1))

interactiveStrategy :: (StatePlayerMonad m, TtyPlayerMonad m) => Player -> m ()
interactiveStrategy player = chooserStrategy player chooseInteractive

prettyTrick trick =
  let prettyOne (player, card) = prettyCard card ++ " from " ++ prettyPlayer player
      pretty [] = ""
      pretty [one] = prettyOne one
      pretty (one : rest) = prettyOne one ++ ", " ++ pretty rest
   in pretty (reverse trick)

-- Spieleridentität drucken
prettyPlayer (Player name) = name

-- | Eine Karte ausdrucken
prettyCard :: Card -> String
prettyCard c = show (rank c) ++ " of " ++ show (suit c)

-- | Liste von Karten ausdrucken
prettyCards :: [Card] -> String
prettyCards cards =
  let loop _ [] = ""
      loop n [card] = prettyOne n card
      loop n (card : cards) = prettyOne n card ++ "\n" ++ loop (n + 1) cards
      prettyOne n card = "(" ++ (show n) ++ ") " ++ prettyCard card
   in loop 1 cards

-- Zahl einlesen
getNumber :: (Num a, Ord a, Read a, Show a, TtyPlayerMonad m) => (a, a) -> m a
getNumber (lo, hi) = do
  s <- readLineM
  let input = read s
  if lo <= input && input <= hi
    then return input
    else do
      writeLineM ("Input must be between " ++ (show lo) ++ " and " ++ (show hi) ++ ". Try again")
      getNumber (lo, hi)

data StateTtyPlayerStep a
  = WaitingForEvent (GameEvent -> (PlayerState, StateTtyPlayer a))
  | WaitingForLine (String -> StateTtyPlayer a)
  | StepDone a

runStateTtyPlayer ::
  Player ->
  StateTtyPlayer a ->
  PlayerState ->
  [GameCommand] ->
  [String] ->
  ([GameCommand], [String], StateTtyPlayerStep a)
runStateTtyPlayer player (Pure result) state commands lines =
  (reverse commands, reverse lines, StepDone result)
runStateTtyPlayer player (Impure (GetEventT cont)) state commands lines =
  (reverse commands, reverse lines, WaitingForEvent (\event -> (playerProcessEvent player event state, cont event)))
runStateTtyPlayer player (Impure (RecordCommandT command cont)) state commands lines =
  runStateTtyPlayer player (cont ()) state (command : commands) lines
runStateTtyPlayer player (Impure (GetPlayerStateT cont)) state commands lines =
  runStateTtyPlayer player (cont state) state commands lines
runStateTtyPlayer player (Impure (ReadLineT cont)) state commands lines =
  (reverse commands, reverse lines, WaitingForLine cont)
runStateTtyPlayer player (Impure (WriteLineT line cont)) state commands lines =
  runStateTtyPlayer player (cont ()) state commands (line : lines)

stateTtyPlayerIO :: Player -> StateTtyPlayer a -> IO (GameEvent -> IO [GameCommand])
stateTtyPlayerIO player playerM =
  do
    let emptyPlayerState = makeEmptyPlayerStateFor player
        (commands0, lines0, step0) = runStateTtyPlayer player playerM emptyPlayerState [] []
    mapM_ putStrLn lines0
    ref <- IORef.newIORef (emptyPlayerState, commands0, step0)
    let handleWaitingForLine =
          do
            (state, commands, step) <- IORef.readIORef ref
            case step of
              WaitingForLine cont ->
                do
                  line <- getLine
                  let playerM' = cont line
                  let (commands', lines', step') = runStateTtyPlayer player playerM' state [] []
                  mapM_ putStrLn lines'
                  IORef.writeIORef ref (state, commands ++ commands', step')
                  handleWaitingForLine
              _ -> return ()
    let processEvent event =
          do
            handleWaitingForLine
            (state, commands, step) <- IORef.readIORef ref
            case step of
              WaitingForEvent cont ->
                do
                  let (state', playerM') = cont event
                  let (commands', lines', step') = runStateTtyPlayer player playerM' state' [] []
                  mapM_ putStrLn lines'
                  IORef.writeIORef ref (state', commands ++ commands', step')
                  handleWaitingForLine
                  (state, commands, step) <- IORef.readIORef ref
                  IORef.writeIORef ref (state, [], step)
                  return commands
              WaitingForLine cont -> error "this can't happen"
              StepDone a -> return commands
    return processEvent


-- Effekte (_nicht nur_ Seiteneffekte) können wir über Monaden modellieren
-- Aber: diese "komponieren" nicht gut