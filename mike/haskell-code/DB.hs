module DB where

import qualified Data.Map.Strict as Map
import Data.Map.Strict (Map, (!))

{-
put "Mike" 100
x = get "Mike"
put "Mike" (x+1)
y = get "Mike"
return (show (x+y))
-}

-- Programm als Objekt

type Key = String
type Value = Integer

-- 1. Versuch:

{-
data DBCommand a =
    Put Key Value
  | Get Key
  | Return a

type DBProgram a = [DBCommand a]

p1 = [
    Put "Mike" 100,
    Get "Mike",
    Put "Mike" (x+1) -- kein x ... blöd
 ]
-}

-- mit Callbacks
data DB a =
    Get Key       (Value -> DB a)
  | Put Key Value (()    -> DB a)
  | Return a

get :: Key -> DB Value
get key = Get key Return -- (\value -> Return value)

put :: Key -> Value -> DB ()
put key value = Put key value Return

-- Callback-Hell
-- Klammern des Grauens
p1 :: DB String
p1 = Put "Mike" 100 (\() ->
     Get "Mike" (\x -> 
     Put "Mike" (x+1) (\() ->
     Get "Mike" (\y ->
     Return (show (x+y))))))

p1' :: DB String
p1' = splice (put "Mike" 100) (\() ->
      splice (get "Mike") (\x ->
      splice (put "Mike" (x+1)) (\() ->
      splice (get "Mike") (\y ->
      Return (show (x+y))))))
      
-- >>> runDB p1' Map.empty
-- ("201",fromList [("Mike",101)])

splice :: DB a -> (a -> DB b) -> DB b
splice (Get key callback) next =
    Get key (\value -> splice (callback value) next)
splice (Put key value callback) next =
    Put key value (\() -> splice (callback ()) next)
splice (Return result) next = next result

-- >>> :info Monad
-- type Monad :: (* -> *) -> Constraint
-- class Applicative m => Monad m where
--   (>>=) :: m a -> (a -> m b) -> m b
--   return :: a -> m a

instance Functor DB where

instance Applicative DB where

instance Monad DB where
    -- "bind"
    (>>=) = splice
    return = Return

p1'' = do put "Mike" 100 -- jede Zeile ein bind
          x <- get "Mike"
          put "Mike" (x+1)
          y <- get "Mike"
          return (show (x+y))

-- >>> runDB p1'' Map.empty
-- ("201",fromList [("Mike",101)])

-- DB-Programm laufenlassen
runDB :: DB a -> Map Key Value -> (a, Map Key Value)
runDB (Get key callback) mp =
    let value = mp ! key
    in runDB (callback value) mp
runDB (Put key value callback) mp =
    let mp' = Map.insert key value mp
    in runDB (callback ()) mp'
runDB (Return result) mp = (result, mp)

-- >>> runDB p1 Map.empty
-- ("201",fromList [("Mike",101)])

io1 = do putStrLn "Mike"
         x <- getLine
         putStrLn ("you typed" ++ x)
         return x