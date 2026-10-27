module ChooserPlayer where

import Cards
import GameEvent (GameCommand (..), GameEvent (..))

data ChooserPlayerOps m =
  ChooserPlayerOps {
     getEventOp :: m GameEvent,
     recordCommandOp :: GameCommand -> m ()
  }

chooserStrategy ::
  Monad m =>
  ChooserPlayerOps m ->
  Player ->
  m Card ->
  m ()
chooserStrategy ops player choose =
  do
    event <- getEventOp ops
    case event of
      HandDealt player' hand ->
        if (player == player') && (containsCard (Card Clubs Two) hand)
          then recordCommandOp ops (PlayCard player (Card Clubs Two))
          else return ()
      PlayerTurnChanged player' ->
        if player == player'
          then do
            card <- choose
            recordCommandOp ops (PlayCard player card)
          else return ()
      LegalCardPlayed player' card -> return ()
      InvalidCardAttempted player' card -> return ()
      TrickTaken player' trick -> return ()
      GameEnded winner -> return ()
    chooserStrategy ops player choose
