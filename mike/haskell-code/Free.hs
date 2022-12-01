{-# LANGUAGE InstanceSigs #-}
module Free where

-- freie Monade
{-
data DB a
  = Get Key (Value -> DB a)
  | Put Key Value (() -> DB a) -- (): "unit", Wert auch ()
  | Return a
-}

{-
data Game a =
    RecordEvent GameEvent (() -> Game a)
  | PlayValid Player Card (Bool -> Game a)
  | TurnOverTrick (Maybe (Trick, Player) -> Game a)
  | PlayerAfter Player (Player -> Game a)
  | GameOver (Maybe Player -> Game a)
  | GetCommand (GameCommand -> Game a)
  | Done a
-}

{-
data DB' self
  = Get Key (Value -> self)
  | Put Key Value (() -> self) -- (): "unit", Wert auch ()
-}

data Free f a = -- DB'
    Pure a -- Done bzw. Return
  | Impure (f (Free f a))

instance Functor (Free f) where

instance Applicative (Free f) where

instance Functor f => Monad (Free f) where
    return = Pure
    (>>=) :: Free f a -> (a -> Free f b) -> Free f b
    (Pure result) >>= next = next result
    (Impure v) >>= next =
        Impure (fmap (>>= next) v) 