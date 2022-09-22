module State where

import Prelude hiding (read)

data State s a =
    -- jeder "Befehl" hat einen Selbstbezug
    Read (s -> State s a)
  | Write s (() -> State s a)
  | Return a

data State' s self =
    Read' (s -> self)
  | Write' (() -> self) 

-- der Selbstbezug auf UniversalMonad
-- d.h. der Selbstbezug muß Typparameter werden
-- c wird dann später "State' s"
data UniversalMonad c a =
    UniversalReturn a
  | UniversalCommand (c (UniversalMonad c a))

type StateMonad s a = UniversalMonad (State' s)


instance (Show s, Show a) => Show (State s a) where
    show (Read callback) = "Read"
    show (Write newState callback) =
        "Write " ++ (show newState)
    show (Return result) =
        "Return " ++ (show result)


read :: State s s
read = Read Return

write :: s -> State s ()
write newValue = Write newValue Return

instance Functor (State s) where

instance Applicative (State s) where

instance Monad (State s) where
    return = Return
    Read callback >>= next =
        Read (\value ->
            callback value >>= next)
    Write newValue callback >>= next =
        Write newValue (\() ->
            callback () >>= next)
    Return result >>= next = 
        next result

p1 :: State Integer String
-- >>> p1
-- Write 17
p1 = do write 17
        x <- read
        write (x+1)
        y <- read
        return (show y)

runState :: State s a -> s -> a
-- >>> runState p1 0
-- "18"
runState (Read callback) state =
    runState (callback state) state
runState (Write newState callback) state =
    runState (callback ()) newState
runState (Return result) state = result

traceState :: State s a -> s -> (a, [s])
-- >>> traceState p1 0
-- ("18",[17,18])
traceState (Read callback) state =
    traceState (callback state) state
traceState (Write newState callback) state =
    let (result, trace) = traceState (callback ()) newState
    in (result, newState : trace)
traceState (Return result) state = 
    (result, [])
