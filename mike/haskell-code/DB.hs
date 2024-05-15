module DB where

import Data.Map.Strict as Map
import Data.Map.Strict (Map, (!))

{-
put "Mike" 10
x = get "Mike"
put "Mike" (x * 2)
y = get "Mike"
return (show (x+y))

-}

type Key = String
type Value = Integer

{-
data DBCommand a =
    Put Key Value
  | Get Key
  | Return a

type DBProgram a = [DBCommand a]

p1 = [Put "Mike" 10,
      x = Get "Mike",
      Put "Mike" (x+ * 2)]
-}

-- "Ein Datenbankprogramm mit Ergebnis vom Typ a"
data DB a =
    Get Key       (Value -> DB a)
  | Put Key Value (()    -> DB a)
  | Return a -- "haben fertig!"

p1 :: DB String
p1 = Put "Mike" 10 (\() ->
     Get "Mike" (\x -> 
     Put "Mike" (x * 2) (\() ->
     Get "Mike" (\y ->
     Return (show (x+y))))))

get :: Key -> DB Value
-- get key = Get key (\value -> Return value)
get key = Get key Return

put :: Key -> Value -> DB ()
put key value = Put key value Return

--        DB a -> (a -> DB b) -> DB b

-- eleganter: a -> DB b  "Kleisli arrows"
compose :: (b -> DB c) -> (a -> DB b) -> (a -> DB c)
compose bc ab a =
    do b <- ab a
       c <- bc b
       return c

splice :: DB a -> (a -> DB b) -> DB b
splice (Get key callback) next =
    Get key (\value -> splice (callback value) next)
splice (Put key value callback) next =
    Put key value (\() -> splice (callback ()) next)
splice (Return result) next = next result

p1' :: DB String
p1' = splice (put "Mike" 10) (\() ->
      splice (get "Mike") (\x ->
      splice (put "Mike" (x*2)) (\() ->
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
    (>>=) = splice -- "bind"
    return = Return

p1'' :: DB String
p1'' = do put "Mike" 10
          x <- get "Mike"
          put "Mike" (x * 2)
          y <- get "Mike"
          return (show (x+y))

runDB :: DB a -> Map Key Value -> (a, Map Key Value)
runDB (Get key callback) mp =
    runDB (callback (mp ! key)) mp
runDB (Put key value callback) mp =
    runDB (callback ()) (Map.insert key value mp)
runDB (Return result) mp = (result, mp)

-- >>> runDB p1'' Map.empty
-- ("30",fromList [("Mike",20)])
