{-# LANGUAGE InstanceSigs #-}
module GameEvent where

import Cards

import qualified Data.Map as Map
import Data.Map (Map)

-- Event:
-- Ein Objekt, das ein Ereignis repräsentiert, das in der Vergangenheit passiert ist.

-- in der Vergangenheit (-> Vergangenheitsform bei der Benamsung)
-- Muster: Nomen + Verb
-- nicht veränderbar ,,, Fakt
-- fachlich (vs. technisch)
-- kein Zustand
-- Redundanz OK

-- -> Event-Sourcing
-- in den Event muß alles drinstehen ... Geschichte der Domäne

-- vs. Commands: Repräsentation eines Wusches, daß etwas in der Zukunft passieren möge

{-
data GameEvent =
    GameStarted Player Player Player Player
  | DeckShuffled [Card] -- gemischten Karten
  | CardsDistributed (Map Player Hand)
  | CardPlayed Card Player
  | RoundPlayed Player Trick -- Integer
  | NextPlayerChanged Player
  | GameEnded Player --- Gewinner

data GameCommand =
    RegisterPlayer Player -- ... oder ...
  | StartGame Player Player Player Player
  | PlayCard Card Player
-}

data GameEvent
  = HandDealt Player Hand
  | PlayerTurnChanged Player
  | LegalCardPlayed Player Card
  | TrickTaken Player Trick
  | IllegalCardAttempted Player Card
  | GameEnded Player
  deriving (Show, Eq)

data GameCommand
  = DealHands (Map Player Hand)
  | PlayCard Player Card
  deriving (Show, Eq)

data Game a =
    RecordEvent GameEvent (() -> Game a) -- wie Put
  | PlayValid Player Card (Bool -> Game a) -- wie Get
  | TurnOverTrick (Maybe (Trick, Player) -> Game a)
  | PlayerAfter Player (Player -> Game a)
  | GameOver (Maybe Player -> Game a)
  | GetCommand (GameCommand -> Game a)
  | Return a

instance Show (Game a) where
    show :: Game a -> String
    show (RecordEvent event _) = "RecordEvent " ++ show event

recordEventM :: GameEvent -> Game ()
recordEventM event = RecordEvent event Return

playValidM :: Player -> Card -> Game Bool
playValidM player card = PlayValid player card Return

turnOverTrickM :: Game (Maybe (Trick, Player))
turnOverTrickM = TurnOverTrick Return

playerAfterM :: Player -> Game Player
playerAfterM player = PlayerAfter player Return

gameOverM :: Game (Maybe Player)
gameOverM = GameOver Return

instance Functor Game where

instance Applicative Game where

instance Monad Game where
    (>>=) :: Game a -> (a -> Game b) -> Game b
    (>>=) (RecordEvent event callback) next =
        RecordEvent event (\() ->
            -- (>>=) (callback ()) next)
            callback () >>= next)
    (>>=) (PlayValid player card callback) next =
        PlayValid player card (\isValid ->
            callback isValid >>= next)
    (>>=) (TurnOverTrick callback) next =
        TurnOverTrick (\turn ->
            callback turn >>= next)
    (>>=) (PlayerAfter player callback) next =
        PlayerAfter player (\nextPlayer ->
            callback nextPlayer >>= next)
    (>>=) (GameOver callback) next =
        GameOver (\winner ->
            callback winner >>= next)
    (>>=) (Return result) next = next result
    return :: a -> Game a
    return = Return

-- Maybe Player: Gewinner:in
tableProcessCommandM :: GameCommand -> Game (Maybe Player)
tableProcessCommandM (DealHands hands) =
    let pairList = Map.toList hands
        events = map (uncurry HandDealt) pairList
        ms = map recordEventM events
    in do sequence_ ms
          return Nothing -- Spiel noch nicht vorbei
tableProcessCommandM (PlayCard player card) =
    do isValid <- playValidM player card
       if isValid
       then do
        recordEventM (LegalCardPlayed player card)
        turnOverTrick <- turnOverTrickM
        case turnOverTrick of
          Just (trick, trickTaker) ->
            do
              recordEventM (TrickTaken trickTaker trick)
              over <- gameOverM
              case over of
                Just winner ->
                  do
                    recordEventM (GameEnded winner)
                    return (Just winner)
                Nothing ->
                  do
                    recordEventM (PlayerTurnChanged trickTaker)
                    return Nothing
          Nothing ->
            do
              nextPlayer <- playerAfterM player
              recordEventM (PlayerTurnChanged nextPlayer)
              return Nothing
       else do
        recordEventM (IllegalCardAttempted player card)
        return Nothing

-- ganzes Spiel
tableLoopM :: GameCommand -> Game Player
tableLoopM command =
    do maybeWinner <- tableProcessCommandM command
       case maybeWinner of
        Nothing -> GetCommand tableLoopM
        Just winner -> return winner


-- $setup
-- >>> let mike = Player "Mike"
-- >>> let nicole = Player "Nicole"
-- >>> let peter = Player "Peter"
-- >>> let annette = Player "Annette"
-- >>> let hands = Map.fromList [(mike, makeHand []), (nicole, makeHand []), (peter, makeHand []), (annette, makeHand [])]

-- >>> tableProcessCommandM (DealHands hands)
-- RecordEvent HandDealt (Player {playerName = "Annette"}) (Hand {unHand = fromList []})
