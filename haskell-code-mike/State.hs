module State where

import Prelude hiding (read)

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

p1 = do write 17
        x <- read
        write (x+1)
        y <- read
        return y
