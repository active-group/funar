module State where

import Prelude hiding (read)

data State a =
    Read (Integer -> State a)
  | Write Integer (() -> State a)
  | Return a

instance Show a => Show (State a) where
    show (Read callback) = "Read"
    show (Write newState callback) =
        "Write " ++ (show newState)
    show (Return result) =
        "Return " ++ (show result)


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

p1 :: State String
-- >>> p1
-- Write 17
p1 = do write 17
        x <- read
        write (x+1)
        y <- read
        return (show y)

runState :: State a -> Integer -> a
-- >>> runState p1 0
-- "18"
runState (Read callback) state =
    runState (callback state) state
runState (Write newState callback) state =
    runState (callback ()) newState
runState (Return result) state = result

traceState :: State a -> Integer -> (a, [Integer])
traceState (Read callback) state =
    traceState (callback state) state
traceState (Write newState callback) state =
    let (result, trace) = traceState (callback ()) newState
    in (result, trace ++ [newState])
traceState (Return result) state = 
    (result, [])