module DB where

import qualified Data.Map as Map
import Data.Map (Map, (!))

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
data DBCommand result =
    Put Key Value
  | Get Key
  | Return result

type DBProgram result = [DBCommand result]

p1 = [
    Put "Mike" 100,
    Get "Mike",
    Put "Mike" (x+1)
 ]
-}

data DB a =
    Get Key       (Value -> DB a) -- Callback / Continuation
  | Put Key Value (()    -> DB a)
  | Return a

p1 :: DB String
p1 =
    Put "Mike" 100 (\() ->
    Get "Mike" (\x ->
    Put "Mike" (x+1) (\() ->
    Get "Mike" (\y ->
    Return (show (x+y))))))

put :: Key -> Value -> DB ()
put key value = Put key value Return

get :: Key -> DB Value
get key = Get key Return

-- Zwei DB-Programme kombinieren
splice :: DB a -> (a -> DB b) -> DB b
splice (Get key callback)       next =
    Get key (\value -> splice (callback value) next)
splice (Put key value callback) next =
    Put key value (\() -> splice (callback ()) next)
splice (Return result)          next = next result

runDB :: DB a -> Map Key Value -> a

-- >>> runDB p1 Map.empty
-- "201"

runDB (Put key value callback) mp = 
    let newMp = Map.insert key value mp 
    in runDB (callback ()) newMp
runDB (Get key callback) mp =
    let value = mp ! key
    in runDB (callback value) mp
runDB (Return result) mp = result

