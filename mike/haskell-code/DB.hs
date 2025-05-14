module DB where

import qualified Data.Map as Map
import Data.Map (Map, (!))

{-
put "Mike" 100
x = get "Mike"
put "Mike" (x+1)
y = get Mike
return (show (x+y))
-}

type Key = String
type Value = Integer

{-
-- Ablauf als Daten
data DBCommand result =
    Put Key Value
  | Get Key
  | Return result

type DBProgram result = [DBCommand result]

p1 = [
    Put "Mike" 100,
    Get "Mike",
    Put "Mike" -- abhängig vom Ergebnis vom Get davor
]
-}

data DB a = -- a statt result
    Get Key       (Value -> DB a) -- Callback/Continuation
  | Put Key Value (()    -> DB a)
  | Return a

p1 :: DB String
p1 = Put "Mike" 100 (\() ->
     Get "Mike" (\x ->
     Put "Mike" (x+1) (\() ->
     Get "Mike" (\y ->
     Return (show(x+y))))))

put :: Key -> Value -> DB ()
put key value = Put key value Return -- (\ value -> Return value)

get :: Key -> DB Value
get key = Get key Return

splice :: DB a -> (a -> DB b) -> DB b
splice (Get key callback) next = 
    Get key (\value ->
        splice (callback value) next)
splice (Put key value callback) next = 
    Put key value (\() -> 
        splice (callback ())    next)
splice (Return result) next = next result

runDB :: DB a -> Map Key Value -> (a, Map Key Value)
runDB (Get key callback) mp = 
    let value = mp ! key
    in runDB (callback value) mp -- tail call
runDB (Put key value callback) mp =
    let mp' = Map.insert key value mp
    in runDB (callback ()) mp'
runDB (Return result) mp = (result, mp)

-- >>> runDB p1 Map.empty
-- ("201",fromList [("Mike",101)])
