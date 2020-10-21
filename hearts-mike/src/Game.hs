module Game where

import qualified Data.Foldable as Foldable

import qualified Data.Set as Set
import Data.Set (Set)

import qualified Data.Map.Strict as Map

import Data.Map.Strict (Map, (!))

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
  let loop player _    [] = player
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


-- * Spiellogik

type Stack = Set Card

-- eingezogene Karten, pro Spieler
type PlayerStacks = Map Player Stack
-- Hände der Spieler
type PlayerHands  = Map Player Hand

data GameState =
  MakeGameState
  { gameStatePlayers :: [Player],
    gameStateNextPlayer :: [Player], -- unendliche Liste, nächste Spieler ist vorn
    gameStateHands   :: PlayerHands,
    gameStateStacks  :: PlayerStacks,
    gameStateTrick   :: Trick
  }
  deriving Show

-- Anfangszustand herstellen
emptyGameState :: [Player] -> GameState
emptyGameState players =
  MakeGameState {
    gameStatePlayers = players,
    gameStateNextPlayer = cycle players,
    gameStateHands = Map.empty,
    gameStateStacks = Map.empty,
    gameStateTrick = emptyTrick
  }

-- ist das Spiel noch am Anfang?
gameAtBeginning :: GameState -> Bool
gameAtBeginning gameState =
  (trickEmpty (gameStateTrick gameState)) && 
    (all null (Map.elems (gameStateStacks gameState)))

-- wer ist als nächstes dran?
playerAfter :: GameState -> Player -> Player
playerAfter state player =
   head (tail (dropTo player (gameStateNextPlayer state)))

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

-- Wert eines Stapels
stackScore :: Stack -> Integer
stackScore hand = sum (map cardScore (Set.toList hand))

-- Wer hat gewonnen?
gameWinner :: GameState -> Player
gameWinner state =
  let playerScores = fmap stackScore (gameStateStacks state)
      cmp (_, score1) (_, score2) = compare score1 score2
  in fst (Foldable.minimumBy cmp (Map.toList playerScores))

-- Event-Sourcing:
-- Log-Buch aller Ereignisse, Ereignis repräsentiert durch Datenobjekt

-- 1. in der Vergangenheit
-- 2. "vollständig"
-- Redundanz OK

{- 
data GameEvent =
    GameBegun
  | CardsShuffled [Card]
  | CardsDealt PlayerHands
  | CardPlayed Player Card
  | TrickTaken Player Trick
  | PlayerTurnChanged Player
  | GameWon Player
-}

data GameEvent
  = HandDealt Player Hand
  | PlayerTurnChanged Player
  | LegalCardPlayed Player Card
  | TrickTaken Player Trick
  | IllegalCardPlayed Player Card
  | GameEnded Player
  deriving (Show)

-- Command: Wunsch, das etwas in der Zukunft passiert
-- !!!!!!! != Event

data GameCommand =
    DealHands PlayerHands
  | PlayCard Player Card
  deriving Show

-- Ereignis in den Zustand einarbeiten
processGameEvent :: GameEvent -> (GameState -> GameState)
processGameEvent (HandDealt player hand) gameState = 
  gameState {
    gameStateHands = Map.insert player hand (gameStateHands gameState)
  }
processGameEvent (PlayerTurnChanged player) state =
  state {
    gameStatePlayers = rotateTo player (gameStatePlayers state)
  }
processGameEvent (LegalCardPlayed player card) gameState =
  gameState {
    gameStateTrick = addToTrick player card (gameStateTrick gameState)
  }
processGameEvent (TrickTaken player trick) gameState = 
  gameState {
    gameStateStacks = 
      let stack = Map.findWithDefault Set.empty player (gameStateStacks gameState)
          newStack = Set.union stack (Set.fromList (cardsOfTrick trick))
      in Map.insert player newStack (gameStateStacks gameState),
    gameStateTrick = emptyTrick
  }
processGameEvent (IllegalCardPlayed player card) gameState = gameState
processGameEvent (GameEnded player) gameState = gameState

-- processGameCommand :: GameCommand -> GameState -> ([GameEvent], GameState)


