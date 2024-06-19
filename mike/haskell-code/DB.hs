{-# LANGUAGE InstanceSigs #-}
module DB where

import qualified Data.Map.Strict as Map
import Data.Map.Strict (Map, (!))
{-
put "Mike" 100
x = get "Mike"
put "Mike" (x+1)
y = get "Mike"
return (show (x + y))
-}

type Key = String

type Value = Integer

{- 
-- gescheitert:

data DBCommand a = -- a: Typ des Ergebnisses, von return
    Put Key Value
  | Get Key 
  | Return a
  deriving Show

type DBProgram a = [DBCommand a]

p1 = [
    Put "Mike" 100,
    Get "Mike",
    Put "Mike" ???
]
-}

-- Namen vergeben: let, =, \

data DB a =
    Get Key       (Value -> DB a) -- callback / continuation
  | Put Key Value (()    -> DB a)
  | Return a

p1 = Put "Mike" 100 (\() ->
     Get "Mike" (\x -> 
     Put "Mike" (x+1) (\() ->
     Get "Mike" (\y ->
     Return (show (x+y))))))

-- - Notation schicker
-- - laufen lassen

-- 1. Idee: Programm zerlegen

-- DB a: (Beschreibung eines) Programms, das mit der Datenbank redet
--       und ein Ergebnis vom Typ a liefert

get :: Key -> DB Value
get key = Get key Return -- (\value -> Return value)

put :: Key -> Value -> DB ()
put key value = Put key value Return -- (\() -> Return ())

splice :: DB a -> (a -> DB b) -> DB b
splice (Get key cont) next =
    Get key       (\value -> splice (cont value) next)
splice (Put key value cont) next =
    Put key value (\() -> splice (cont ()) next)
splice (Return result) next = next result

p1' :: DB String
p1' = splice (put "Mike" 100) (\() ->
      splice (get "Mike") (\x -> 
      splice (put "Mike" (x+1)) (\() ->
      splice (get "Mike") (\y ->
      Return (show (x+y))))))

-- >>> :info Monad
-- type Monad :: (* -> *) -> Constraint
-- class Applicative m => Monad m where
--   (>>=) :: m a -> (a -> m b) -> m b
--   return :: a -> m a

instance Functor DB where

instance Applicative DB where

instance Monad DB where
    (>>=) :: DB a -> (a -> DB b) -> DB b
    (>>=) = splice
    return :: a -> DB a
    return = Return

p1'' :: DB String
p1'' = do put "Mike" 100
          x <- get "Mike"
          put "Mike" (x+1)
          y <- get "Mike"
          return (show (x+y))

-- >>> runDB p1'' Map.empty
-- ("201",fromList [("Mike",101)])

runDB :: DB a -> Map Key Value -> (a, Map Key Value)
runDB (Get key cont) mp =
    let value = mp ! key 
    in runDB (cont value) mp
runDB (Put key value cont) mp =
    let mp' = Map.insert key value mp
    in runDB (cont ()) mp'
runDB (Return result) mp = (result, mp)

