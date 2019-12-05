{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleContexts #-}
module State where


{-
Zustand ist ein Int:

data State result = ...

write  :: Int -> State ()
yank :: State Int

-}

data State state result =
    State (state -> (result, state))

write :: state -> State state ()
write newState =
    State (\ oldState ->
            ((), newState))

yank :: State state state
yank = State (\ state -> (state, state))

instance Functor (State state) where

instance Applicative (State state) where

instance Monad (State state) where
    -- return :: a -> State state a
    return a = State (\state -> (a, state))
    -- (>>=) :: State state a -> (a -> State state b) -> State state b
    (>>=) (State t) f =
        State (\ state0 ->
                 let (a, state1) = t state0
                     -- f a :: State state b
                     (State tb) = f a -- tb :: state -> (b, state)
                     (b, state2) = tb state1
                 in (b, state2))              

p = do write 5
       x <- yank
       write (x + 1)
       y <- yank
       return y

p' = write 5 >>= \ () ->
     yank >>= \ x ->
     write (x + 1) >>= \ () ->
     yank >>= \ y ->
     return y

-- 1. Idee: Interface statt Implementierung
-- 2. Idee: Monaden kombinierbar machen

-- Interface für Monaden, die Zustand können
class Monad monad => MonadState state (monad :: * -> *) | monad -> state where
    get :: monad state
    put :: state -> monad ()

instance MonadState state (State state) where
    get = yank
    put = write

p'' :: MonadState Integer monad => monad String
p'' = do put 5
         x <- get
         put (x + 1)
         return (show x)

data StateT state monad result =
    StateT (state -> monad (result, state))