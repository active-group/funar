module Free where

{-
data DB a
        ^
  = Get Key (Value -> DB a) -- Callback / Continuation
                   ^^^^^^^
  | Put Key Value (() -> DB a)
  | Return a
  ^^^^^^^^^^

data Game a
  = PlayValid Player Card (Bool -> Game a)
  | RecordEvent GameEvent (() -> Game a)
  | TurnOverTrick (Maybe (Trick, Player) -> Game a)
  | PlayerAfter Player (Player -> Game a)
  | GameOver (Maybe Player -> Game a)
  | GetCommand (GameCommand -> Game a)
  | Done a
-}

