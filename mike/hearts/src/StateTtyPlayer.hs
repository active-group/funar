{-# LANGUAGE FlexibleInstances #-}

module StateTtyPlayer where

import GameEvent (GameCommand (..), GameEvent (..))
import ChooserPlayer
import StatePlayer (PlayerState (..), playerProcessEvent, makeEmptyPlayerStateFor)
import Cards

import Free
import Data.IORef (IORef)
import qualified Data.IORef as IORef

data StateTtyPlayer' r
  = GetEvent (GameEvent -> r)
  | RecordCommand GameCommand (() -> r)
  | GetPlayer (Player -> r)
  | GetHand (Hand -> r)
  | GetTrick (Trick -> r)
  | ReadLine (String -> r)
  | WriteLine String (() -> r)

instance Functor StateTtyPlayer' where
  fmap f (GetEvent cont) = GetEvent (\event -> f (cont event))
  fmap f (RecordCommand command cont) = RecordCommand command (\() -> f (cont ()))
  fmap f (GetPlayer cont) = GetPlayer (\state -> f (cont state))
  fmap f (GetHand cont) = GetHand (\state -> f (cont state))
  fmap f (GetTrick cont) = GetTrick (\state -> f (cont state))
  fmap f (ReadLine cont) = ReadLine (\line -> f (cont line))
  fmap f (WriteLine line cont) = WriteLine line (\() -> f (cont ()))

type StateTtyPlayer a = Free StateTtyPlayer' a

getEventM :: StateTtyPlayer GameEvent
getEventM = Impure (GetEvent Return)

recordCommandM :: GameCommand -> StateTtyPlayer ()
recordCommandM command = Impure (RecordCommand command Return)

getPlayerM :: StateTtyPlayer Player
getPlayerM = Impure (GetPlayer Return)

getHandM :: StateTtyPlayer Hand
getHandM = Impure (GetHand Return)

getTrickM :: StateTtyPlayer Trick
getTrickM = Impure (GetTrick Return)

chooserPlayerOps :: ChooserPlayerOps (Free StateTtyPlayer')
chooserPlayerOps = ChooserPlayerOps getEventM recordCommandM

readLineM = Impure (ReadLine Return)
writeLineM line = Impure (WriteLine line Return)

-- interaktiver Spieler
chooseInteractive :: StateTtyPlayer Card
chooseInteractive =
  do
    player <- getPlayerM
    writeLineM ("Your turn, player " ++ (playerName player))
    trick <- getTrickM
    case trickToList trick of
      [] ->
        writeLineM "You lead the next trick."
      trick ->
        writeLineM ("Trick: " ++ prettyTrick trick)
    hand <- getHandM
    let cards = handCards hand
        ncards = length cards
    writeLineM ("Your hand:\n" ++ prettyCards cards)
    writeLineM ("Pick a card (1-" ++ show ncards ++ ")")
    selected <- getNumber (1, ncards)
    return (cards !! (selected - 1))

interactiveStrategy :: Player -> StateTtyPlayer ()
interactiveStrategy player = chooserStrategy chooserPlayerOps player chooseInteractive

prettyTrick trick =
  let prettyOne (player, card) = prettyCard card ++ " from " ++ prettyPlayer player
      pretty [] = ""
      pretty [one] = prettyOne one
      pretty (one : rest) = prettyOne one ++ ", " ++ pretty rest
   in pretty (reverse trick)

-- Spieleridentität drucken
prettyPlayer :: Player -> String
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
getNumber :: (Num a, Ord a, Read a, Show a) => (a, a) -> StateTtyPlayer a
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
  Player -> StateTtyPlayer a -> (PlayerState, [GameCommand], [String]) ->
    (StateTtyPlayerStep a, [GameCommand], [String])
runStateTtyPlayer player (Return result) (state, commands, lines) =
  (StepDone result, reverse commands, reverse lines)
runStateTtyPlayer player (Impure (GetEvent cont)) (state, commands, lines) =
  (WaitingForEvent (\event -> (playerProcessEvent event state, cont event)), reverse commands, reverse lines)
runStateTtyPlayer player (Impure (RecordCommand command cont)) (state, commands, lines) =
  runStateTtyPlayer player (cont ()) (state, command : commands, lines)
runStateTtyPlayer player (Impure (GetPlayer cont)) s@(state, _, _) =
  runStateTtyPlayer player (cont (playerStatePlayer state)) s
runStateTtyPlayer player (Impure (GetHand cont)) s@(state, _, _) =
  runStateTtyPlayer player (cont (playerStateHand state)) s
runStateTtyPlayer player (Impure (GetTrick cont)) s@(state, _, _) =
  runStateTtyPlayer player (cont (playerStateTrick state)) s
runStateTtyPlayer player (Impure (ReadLine cont)) (state, commands, lines) =
  (WaitingForLine cont, reverse commands, reverse lines)
runStateTtyPlayer player (Impure (WriteLine line cont)) (state, commands, lines) =
  runStateTtyPlayer player (cont ()) (state, commands, line : lines)

stateTtyPlayerIO :: Player -> StateTtyPlayer a -> IO (GameEvent -> IO [GameCommand])
stateTtyPlayerIO player playerM =
  do
    let emptyPlayerState = makeEmptyPlayerStateFor player
        (step0, commands0, lines0) = runStateTtyPlayer player playerM (emptyPlayerState, [], [])
    mapM_ putStrLn lines0
    ref <- IORef.newIORef (step0, emptyPlayerState, commands0)
    let handleWaitingForLine =
          do
            (step, state, commands) <- IORef.readIORef ref
            case step of
              WaitingForLine cont ->
                do
                  line <- getLine
                  let playerM' = cont line
                  let (step', commands', lines') = runStateTtyPlayer player playerM' (state, [], [])
                  mapM_ putStrLn lines'
                  IORef.writeIORef ref (step', state, commands ++ commands')
                  handleWaitingForLine
              _ -> return ()
    let processEvent event =
          do
            handleWaitingForLine
            (step, state, commands) <- IORef.readIORef ref
            case step of
              WaitingForEvent cont ->
                do
                  let (state', playerM') = cont event
                  let (step', commands', lines') = runStateTtyPlayer player playerM' (state', [], [])
                  mapM_ putStrLn lines'
                  IORef.writeIORef ref (step', state', commands ++ commands')
                  handleWaitingForLine
                  (step, state, commands) <- IORef.readIORef ref
                  IORef.writeIORef ref (step, state, [])
                  return commands
              WaitingForLine cont -> error "this can't happen"
              StepDone a -> return commands
    return processEvent
