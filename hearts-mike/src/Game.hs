module Game where

import qualified Data.Foldable as Foldable

import qualified Data.Set as Set
import Data.Set (Set)

import qualified Data.Map.Strict as Map

import Data.Map.Strict (Map, (!))

import Control.Monad.State.Strict as State
import Control.Monad.State.Strict (State)
import Control.Conditional

import Cards

import Debug.Trace (trace)

-- start card
twoOfClubs = Card Clubs (Numeric 2)

-- Games
data Player = Player { playerId :: String, playerName :: String }

instance Show Player where
  show (Player id name) = name ++ "(" ++ id ++ ")"

instance Eq Player where
  Player id1 _ == Player id2 _ = id1 == id2

instance Ord Player where
  compare (Player id1 _) (Player id2 _) = compare id1 id2

-- * Tricks

-- last card is at the front
type Trick = [(Player, Card)]

-- leeren Stich herstellen
emptyTrick :: Trick
emptyTrick = []

-- ist Stich leer
trickEmpty :: Trick -> Bool
trickEmpty trick = null trick

-- wie groß ist der Stich?
trickSize :: Trick -> Int
trickSize trick = length trick

-- alle Karten desStich
cardsOfTrick :: Trick -> [Card]
cardsOfTrick trick = map snd trick

-- Karte auf den Stich legen
addToTrick :: Player -> Card -> Trick -> Trick
addToTrick player card trick = (player, card) : trick

-- die Karte des Stich, die bedient werden muß
leadingCardOfTrick :: Trick -> Card
leadingCardOfTrick trick = snd (last trick)

-- wer muß den Stich einziehen?
whoTakesTrick :: Trick -> Player
whoTakesTrick [] = error "trick is empty"
whoTakesTrick trick =
  let loop player _ [] = player
      loop player card ((player', card') : rest) =
        if cardBeats card' card
        then loop player' card' rest
        else loop player card rest
      (player0, card0) : rest' = reverse trick
  in loop player0 card0 rest'

-- ist die Karte aus der Hand für den Stich zulässig ?
legalCard :: Card -> Hand -> Trick -> Bool
legalCard card hand trick =
  containsCard card hand &&
  case trick of
    [] -> True -- if trick is empty, then any card on hand is fine
    _ -> let (_, firstCard) = last trick
             firstSuit = suit firstCard
         in  suit card == firstSuit -- ok if suit is followed
             || all ((/= firstSuit) . suit) hand -- ok if no such suit in hand

-- Wert einer Karte
cardScore :: Card -> Integer
cardScore (Card Spades Queen) = 13
cardScore (Card Hearts _) = 1
cardScore _ = 0

-- * 

-- Liste rotieren
rotate :: [a] -> [a]
rotate (x : xs) = xs ++ [x]
rotate [] = undefined

-- Liste zu einem bestimmten Element rotieren
rotateTo :: Eq a => a -> [a] -> [a]
rotateTo y xs@(x : xs') | x == y = xs
                        | otherwise = rotateTo y (xs' ++ [x])
rotateTo y [] = undefined

-- * Spiellogik

-- wie data, aber ohne Laufzeit-Overhead
newtype Stack = Stack (Set Card)
  deriving Show

emptyStack :: Stack
emptyStack = Stack Set.empty

stackEmpty :: Stack -> Bool
stackEmpty (Stack cards) = null cards

stackAddCards :: Stack -> [Card] -> Stack
stackAddCards (Stack cards) moreCards =
  Stack (Set.union cards (Set.fromList moreCards))

-- Wert eines Stapels
stackScore :: Stack -> Integer
stackScore (Stack cards) = sum (map cardScore (Set.toList cards))

-- eingezogene Karten, pro Spieler
type PlayerStacks = Map Player Stack
-- Hände der Spieler
type PlayerHands  = Map Player Hand

data GameState =
  GameState
  { gameStatePlayers :: [Player], -- wer dran ist, steht vorn
    gameStateHands   :: PlayerHands,
    gameStateStacks  :: PlayerStacks,
    gameStateTrick   :: Trick
  }
  deriving Show

-- Anfangszustand herstellen
emptyGameState :: [Player] -> GameState
emptyGameState players =
  GameState {
    gameStatePlayers = players,
    gameStateHands = Map.empty,
    gameStateStacks = Map.fromList (map (\ player -> (player, emptyStack)) players),
    gameStateTrick = emptyTrick
  }

-- ist das Spiel noch am Anfang?
gameAtBeginning :: GameState -> Bool
gameAtBeginning gameState =
  (trickEmpty (gameStateTrick gameState)) && 
    (all stackEmpty (Map.elems (gameStateStacks gameState)))

-- wer ist als nächstes dran?
playerAfter :: GameState -> Player -> Player
playerAfter state player =
   head (rotate (rotateTo player (gameStatePlayers state)))

-- wer ist gerade dran?
currentPlayer state =
  head (gameStatePlayers state)

-- ist es zulässig, diese Karte auszuspielen?
playValid :: GameState -> Player -> Card -> Bool
playValid gameState player card =
  let hand = gameStateHands gameState ! player
      trick = gameStateTrick gameState
  in
  legalCard card hand trick &&
  if gameAtBeginning gameState
  then card == twoOfClubs
  else currentPlayer gameState == player

-- ist das Spiel vorbei?
gameOver :: GameState -> Bool
gameOver state = all isHandEmpty (Map.elems (gameStateHands state))

-- ist diese Runde vorbei?
turnOver :: GameState -> Bool
turnOver state =
  length (gameStatePlayers state) == trickSize (gameStateTrick state)


-- Wer hat gewonnen?
gameWinner :: GameState -> Maybe Player
gameWinner state =
  let playerScores = fmap stackScore (gameStateStacks state)
      cmp (_, score1) (_, score2) = compare score1 score2
  in if not (gameOver state) || Map.null playerScores
     then Nothing
     else Just (fst (Foldable.minimumBy cmp (Map.toList playerScores)))

gameWinner' :: GameState -> Player
gameWinner' state =
  let playerScores = fmap stackScore (gameStateStacks state)
      cmp (_, score1) (_, score2) = compare score1 score2
  in fst (Foldable.minimumBy cmp (Map.toList playerScores))

{-
-- direkte Funktionen für den Spielablauf:
playerTakesTrick :: Player -> GameState -> GameState
playerPlaysCard :: Player -> Card -> GameState -> GameState
-}

-- Events
-- - beschreiben alles, was passiert ist
-- - fachlich motiviert
-- - in der Vergangenheit

-- vs. Commands
-- - Wünsche für die Zukunft

{- Übung:

data GameEvent =
    GameOver Player
  | PlayerPlayedCard Player Card
  | TrickIsFull Trick
  | TrickTaken Player Trick
  | GameStarted PlayerHands
  | PlayerArrived Player
  | TurnChanged Player

data GameCommand =
    PlayCard Player Card
  | TakeTrick Player
  | DealHands PlayerHands

-}

data GameEvent =
    HandDealt Player Hand
  | PlayerTurnChanged Player
  | LegalCardPlayed Player Card
  | TrickTaken Player Trick
  | IllegalCardPlayed Player Card
  | GameEnded Player
  deriving Show

data GameCommand =
    DealHands PlayerHands
  | PlayCard Player Card
  deriving Show

-- Effekt eines Events auf GameState berechnen

-- Karte ausspielen
takeCard :: PlayerHands -> Player -> Card -> PlayerHands
takeCard playerHands player card =
  Map.alter (fmap (removeCard card)) player playerHands

-- Karten zum Stapel hinzufügen
addToStack :: PlayerStacks -> Player -> [Card] -> PlayerStacks
addToStack playerStacks player cards =
  let playerStack = Map.findWithDefault emptyStack player playerStacks
  in Map.insert player (stackAddCards playerStack cards) playerStacks

-- Ereignis in den Zustand einarbeiten
processGameEvent :: GameEvent -> GameState -> GameState
-- processGameEvent event state | trace ("processGameEvent " ++ show state ++ " " ++ show event) False = undefined
processGameEvent (HandDealt player hand) state =
  state {
    gameStateHands = Map.insert player hand (gameStateHands state)
  }
processGameEvent (PlayerTurnChanged player) state =
  state {
    gameStatePlayers  = rotateTo player (gameStatePlayers state)
  }
processGameEvent (LegalCardPlayed player card) state =
  state {
    gameStateHands = takeCard (gameStateHands state) player card,
    gameStateTrick = addToTrick player card (gameStateTrick state)
  }
processGameEvent (TrickTaken player trick) state =
  state {
    gameStateStacks =
      addToStack (gameStateStacks state) player (cardsOfTrick trick),
    gameStateTrick = emptyTrick
  }
processGameEvent (IllegalCardPlayed player card) state = state
processGameEvent (GameEnded player) state = state


-- Events ermitteln, die von einem Command verursacht werden
processGameCommand :: GameCommand -> GameState -> [GameEvent]
processGameCommand (DealHands hands) state =
  map (uncurry HandDealt) (Map.toList hands)
processGameCommand (PlayCard player card) state =
  if playValid state player card
  then
    let event1 = LegalCardPlayed player card
        state1 = processGameEvent event1 state
    in if turnOver state1
       then 
         let trick = gameStateTrick state1
             trickTaker = whoTakesTrick trick
             event2 = TrickTaken trickTaker trick
             state2 = processGameEvent event2 state1
             event3 = case gameWinner state2 of
                        Nothing -> PlayerTurnChanged trickTaker
                        Just winner -> GameEnded winner
         in [event1, event2, event3]
       else 
         let event2 = PlayerTurnChanged (playerAfter state1 player)
         in [event1, event2]
  else
    [IllegalCardPlayed player card]

-- State state a
-- ^^^^^^^^^^^ die Monade
-- State.get :: State state state
-- State.put :: state -> State state ()

processGameEventM' :: GameEvent -> State GameState ()
processGameEventM' event =
  do gameState <- State.get
     let gameState' = processGameEvent event gameState
     State.put gameState'      

processGameCommandM' :: GameCommand -> State GameState [GameEvent]
processGameCommandM' (DealHands hands) =
  return (map (uncurry HandDealt) (Map.toList hands))
processGameCommandM' (PlayCard player card) =
  do let playValid' player card state = playValid state player card
     ifM (playValid' player card <$> State.get)
       (do let event1 = LegalCardPlayed player card
           processGameEventM' event1
           ifM (turnOver <$> State.get) -- State GameState Bool
             (do trick <- gameStateTrick <$> State.get
                 let trickTaker = whoTakesTrick trick
                     event2 = TrickTaken trickTaker trick
                 processGameEventM' event2
                 event3 <- ifM (gameOver <$> State.get)
                            (GameEnded <$> (gameWinner' <$> State.get))
                            (return (PlayerTurnChanged trickTaker))
                 return [event1, event2, event3])
             (do event2 <- PlayerTurnChanged <$> ((flip playerAfter player) <$> State.get)
                 return [event1, event2]))
       (return [IllegalCardPlayed player card])

