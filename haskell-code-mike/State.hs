module State where

data State a =
    Read (Integer -> State a)
  | Write Integer (() -> State a)
  | Return a

read :: State Integer
read = Read Return

write :: Integer -> State ()
write newValue = Write newValue Return

instance Functor State where

instance Applicative State where

instance Monad State where
    return = Return
    Read callback >>= next =
        Read (\value ->
            callback value >>= next)
    Write newValue callback >>= next =
        Write newValue (\() ->
            callback () >>= next)
    Return result >>= next = 
        next result
