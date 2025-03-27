{-# LANGUAGE InstanceSigs #-}
module DB where

import qualified Data.Map.Strict as Map
import Data.Map.Strict (Map, (!))

{-
put "Mike" 50
x = get "Mike"
put "Mike" (x+1)
y = get "Mike"
return (show (x + y))
-}

type Key = String
type Value = Integer

{-
data DBCommand result =
    Put Key Value
  | Get Key
  | Return result

type DBProgram result = [DBCommand result]

p1 = [Put "Mike" 50,
      Get "Mike"] -- ups
-}
data DB a =
    Get Key (Value -> DB a) -- Callback / Continuation
  | Put Key Value (() -> DB a)
  | Return a

p1 :: DB String
p1 = Put "Mike" 50 (\() ->
     Get "Mike" (\x -> 
     Put "Mike" (x+1) (\() ->
     Get "Mike" (\y ->
     Return (show (x+y))))))

-- fehlt noch:
-- 1. laufenlassen
-- 2. schick aussehen (ohne viele Klammern)

get :: Key -> DB Value
get key = Get key Return

put :: Key -> Value -> DB ()
put key value = Put key value Return

p1' = put "Mike" 50 -- ...


splice :: DB a -> (a -> DB b) -> DB b
splice (Get key cont) next =
    Get key (\value -> splice (cont value) next)
splice (Put key value cont) next =
    Put key value (\() -> splice (cont ()) next)
splice (Return result) next = next result

-- >>> :info Monad
-- type Monad :: (* -> *) -> Constraint
-- class Applicative m => Monad m where
--   (>>=) :: m a -> (a -> m b) -> m b
--   return :: a -> m a

p1'' :: DB String
p1'' = splice (put "Mike" 50) (\() ->
       splice (get "Mike") (\ x ->
       splice (put "Mike" (x+1)) (\() ->
       splice (get "Mike") (\ y ->
       Return (show (x+y))))))

-- fmap ::       (a ->    b) -> DB a -> DB b
-- (<*>) ::   DB (a ->    b) -> DB a -> DB b
-- flip (>>=) :: (a -> DB b) -> DB a -> DB b



instance Functor DB where
    fmap :: (a -> b) -> DB a -> DB b
    fmap f (Get key cont) =
        Get key (\value -> fmap f (cont value))
    fmap f (Put key value cont) =
        Put key value (\() -> fmap f (cont ()))
    fmap f (Return result) = Return (f result)

instance Applicative DB where
    pure :: a -> DB a
    pure = Return
    (<*>) :: DB (a -> b) -> DB a -> DB b
    (<*>) = undefined

instance Monad DB where
    (>>=) :: DB a -> (a -> DB b) -> DB b
    (>>=) = splice -- "bind"
    return :: a -> DB a
    return = Return

p1''' :: DB String
p1''' = do put "Mike" 50
           x <- get "Mike"
           put "Mike" (x+1)
           y <- get "Mike"
           return (show (x+y))

-- >>> runDB p1 Map.empty
-- ("101",fromList [("Mike",51)])

-- >>> runDB p1''' Map.empty
-- ("101",fromList [("Mike",51)])

runDB :: DB a -> Map Key Value -> (a, Map Key Value)
runDB (Get key cont) mp = 
    let value = mp ! key
    in runDB (cont value) mp
runDB (Put key value cont) mp =
    let mp' = Map.insert key value mp
    in runDB (cont ()) mp'
runDB (Return result) mp = (result, mp)
