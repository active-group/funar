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

type Key = String
type Value = Integer

data DB' self =
    Get Key (Value -> self)
  | Put Key Value (() -> self)

data Free m' a =
    Pure a -- für Done/Return
  | Impure (m' (Free m' a)) 

instance Functor (Free m') where

instance Applicative (Free m') where

instance Functor m' => Monad (Free m') where
    return = Pure

    (>>=) (Pure a) next = next a
    (>>=) (Impure f) next = 
        Impure (fmap (>>= next) f)

