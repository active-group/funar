module GameEvent where

import Cards

import qualified Data.Map as Map
import Data.Map (Map)

-- Event: Repräsentation eines Ereignisses, das passiert ist

-- in der Vergangenheit passiert
-- steht alles drin, was wir wissen / was relevant ist
-- Redundanz OK
-- fachlich getrieben

{-
data GameEvent =
    CardsShuffled [Card]
  | CardsDealt Hand Hand Hand Hand
  | CardPlayed Card Player
  | TrickTaken Trick Player
  | GameWon Player Integer

-- Command: Wunsch, daß etwas passieren soll
data GameCommand =
    PlayCard Card Player
-}

-- Antipattern: Events und Commands sind das gleiche
-- Antipattern:

-- eigentlich:
-- f :: Input1 -> Input2 -> Input3 -> Output
-- f input1 input2 input3 = output

-- ->

-- data MkfInputCommand { input1 :: Input1, input2 :: Input2, input3 :: Input3}
-- f command = ... input1 command ... input2 command ... input3 command

-- f (MkfInputCommand input1 input2 input3)

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

-- "Das Spiel" macht aus Commands Abläufe, die Events generieren

data Game a =
    RecordEvent GameEvent (() -> Game a)
  | GetCommand (GameCommand -> Game a) 
  | PlayValid Card Player (Bool -> Game a)
  | TurnOverTrick (Maybe (Trick, Player) -> Game a)
  | PlayerAfter Player (Player -> Game a)
  | GameOver (Maybe Player -> Game a)
  | Done a -- "return"

recordEventM :: GameEvent -> Game ()
recordEventM event = RecordEvent event Done

playValidM :: Card -> Player -> Game Bool
playValidM card player = PlayValid card player Done

turnOverTrickM :: Game (Maybe (Trick, Player))
turnOverTrickM = TurnOverTrick Done

playerAfterM :: Player -> Game Player
playerAfterM player = PlayerAfter player Done

gameOverM :: Game (Maybe Player)
gameOverM = GameOver Done

instance Functor Game where
instance Applicative Game where

instance Monad Game where
    RecordEvent event callback >>= next     =
        RecordEvent event (\() -> callback () >>= next)
    PlayValid card player callback >>= next =
        PlayValid card player (\valid -> callback valid >>= next)
    TurnOverTrick callback >>= next = 
        TurnOverTrick (\trick -> callback trick >>= next)
    PlayerAfter player callback >>= next =
        PlayerAfter player (\player -> callback player >>= next)
    GameOver callback >>= next =
        GameOver (\maybeWinner -> callback maybeWinner >>= next)
    Done result >>= next                    = next result
    return = Done
    
tableProcessCommand :: GameCommand -> Game (Maybe Player) -- ggf kommt hier Gewinner:in raus
tableProcessCommand (DealHands hands) =
    let handsList = Map.toList hands
        eventsList = map (uncurry HandDealt) handsList
    in do mapM_ recordEventM eventsList -- mapM :: (a -> Game b) -> [a] -> Game ()
          return Nothing -- Spiel noch nicht zu Ende
tableProcessCommand (PlayCard player card) =
    do valid <- playValidM card player
       if valid
       then do recordEventM (LegalCardPlayed player card)
               turnOverTrick <- turnOverTrickM
               case turnOverTrick of
                Just (trick, trickTaker) ->
                    do recordEventM (TrickTaken trickTaker trick)
                       over <- gameOverM
                       case over of
                         Just winner ->
                           do recordEventM (GameEnded winner)
                              return (Just winner)
                         Nothing ->
                           do recordEventM (PlayerTurnChanged trickTaker)
                              return Nothing
                Nothing ->
                    do nextPlayer <- playerAfterM player
                       recordEventM (PlayerTurnChanged nextPlayer)
                       return Nothing
       else do recordEventM (IllegalCardAttempted player card)
               return Nothing

--    playValidM card player >>= \ valid -> return undefined

tableLoopM :: GameCommand -> Game Player
tableLoopM command =
  do maybeWinner <- tableProcessCommand command
     case maybeWinner of
      Nothing -> GetCommand tableLoopM
      Just winner -> return winner