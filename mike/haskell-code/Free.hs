module Free where

data Free f a =
    Pure a -- Return
  | Impure (f (Free f a))

type Key = String
type Value = Integer

data DB' self = 
    Get Key (Value -> self) -- ...
  | Put Key Value (() -> self)-- ...

instance Functor DB' where
    fmap f (Put key value callback) =
        Put key value (\() -> f (callback ()))
    fmap f (Get key callback) =
        Get key (\value -> f (callback value))

type DB a = Free DB' a

instance Functor (Free f) where

instance Applicative (Free f) where

instance Functor f => Monad (Free f) where
    return = Pure
    (>>=) (Pure result) next = next result
    (>>=) (Impure command) next = 
        Impure (fmap (>>= next) command)