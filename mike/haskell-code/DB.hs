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

type Key = String
type Value = Integer

{-
data DBCommand a = 
    Put Key Value
  | Get Key
  | Return a

type DBProgram a = [DBCommand a]

p1 = [Put "Mike" 100,
      Get "Mike"]
-}
data DB a =
    Get Key       (Value -> DB a) -- Callback / Continuation
  | Put Key Value (()    -> DB a)
  | Return a

get :: Key -> DB Value
get key = Get key Return -- (\value -> Return value)

put :: Key -> Value -> DB ()
put key value = Put key value Return

splice :: DB a -> (a -> DB b) -> DB b
splice (Get key callback) next =
    Get key (\value -> splice (callback value) next)
splice (Put key value callback) next =
    Put key value (\() -> splice (callback ()) next)
splice (Return result) next = next result

p1 :: DB String
p1 = Put "Mike" 100 (\() ->
     Get "Mike" (\x ->
     Put "Mike" (x+1) (\() ->
     Get "Mike" (\y ->
     Return (show (x+y))))))

-- DB-Programm ausführen
executeDB :: DB a -> Map Key Value -> (a, Map Key Value)

-- >>> executeDB p1 Map.empty
-- ("201",fromList [("Mike",101)])

executeDB (Get key callback) mp = 
    let value = mp ! key
    in executeDB (callback value) mp
executeDB (Put key value callback) mp = 
    let updated = Map.insert key value mp
    in executeDB (callback ()) updated
executeDB (Return result) mp = (result, mp)
