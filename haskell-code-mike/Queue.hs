module Queue where

{-
type Queue a = [a]

addQueue :: a -> Queue a -> Queue a
addQueue a q = q ++ [a]

-}

data Queue a = Queue { pop :: [a], push :: [a] }
  deriving Show

emptyQueue = Queue [] []

addQueue :: a -> Queue a -> Queue a
addQueue a q = q { push = a : (push q) }

removeQueue :: Queue a -> Maybe (a, Queue a)
removeQueue (Queue (pop:pops) push) =
    Just (pop, Queue pops push)
removeQueue (Queue [] []) = Nothing
removeQueue (Queue [] push) =
    removeQueue (Queue (reverse push) [])

q1 = addQueue 1 emptyQueue
q2 = addQueue 2 q1
q3 = addQueue 3 q2
