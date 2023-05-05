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

instance Functor DB' where
    fmap f (Get key callback) =
        Get key (\value -> f (callback value))
    fmap f (Put key value callback) =
        Put key value (\() -> f (callback ()))

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

runFree :: Monad m => (f (Free f a) -> (Free f a -> m a) -> m a) -> Free f a -> m a
runFree _ (Pure result) = return result
runFree rrr (Impure command) = rrr command (runFree rrr)

type DB a = Free DB' a

-- get
-- put
-- runDB