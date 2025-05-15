{-# LANGUAGE InstanceSigs #-}
module Free where

{-
-- abstrahieren über:
data DB a -- a statt result
  = Get Key (Value -> DB a) -- Callback/Continuation
  | Put Key Value (() -> DB a)
  | Return a

data Game a
  = RecordEvent GameEvent (() -> Game a) -- wie Put
  | PlayValid Player Card (Bool -> Game a) -- wie Get
  | TurnOverTrick (Maybe (Trick, Player) -> Game a)
  | PlayerAfter Player (Player -> Game a)
  | GameOver (Maybe Player -> Game a)
  | GetCommand (GameCommand -> Game a)
  | Return a
-}

type Key = String
type Value = Integer

data DB' self =
    Get Key (Value -> self) -- ??? Selbstbezug, hier unbekannt
  | Put Key Value (() -> self)
{-
data Game' a
  = RecordEvent GameEvent (() -> ???) -- wie Put
  | PlayValid Player Card (Bool -> ???) -- wie Get
  | TurnOverTrick (Maybe (Trick, Player) -> ???)
  | PlayerAfter Player (Player -> ???)
  | GameOver (Maybe Player -> ???)
  | GetCommand (GameCommand -> ???) -- ??? Selbstbezug
-}

data Free f a = --- f steht für DB', Game'
    Impure (f (Free f a)) --- <- hier muß dann DB' / Game'
  | Return a

type DB a = Free DB' a  -- Effekt-Libraries: Free '[DB' ...] a

instance Functor (Free f) where

instance Applicative (Free f) where


instance Functor f => Monad (Free f) where
    return :: a -> Free f a
    return = Return

    (>>=) :: Free f a -> (a -> Free f b) -> Free f b
    (>>=) (Return result) next = next result
    (>>=) (Impure f) next =
        Impure (fmap (>>= next) f)

instance Functor DB' where
    fmap :: (a -> b) -> DB' a -> DB' b
    fmap f (Get key callback) = 
        Get key (\value -> f (callback value))
    fmap f (Put key value callback) = 
        Put key value (\() -> f (callback ()))