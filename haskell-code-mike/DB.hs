module DB where

-- die Namen fangen alle mit Map.
import qualified Data.Map.Strict as Map
import Data.Map (Map, (!))

{-
put "Mike" 51
x = get "Mike"
put "Mike" (x + 1)
y = get "Mike"
return x + y
-}

{-
data DBCommand a =
    Put String Integer
  | Get String
  | Return a

type DBProgram a = [DBCommand a]

p1 :: DBProgram Integer
p1 = [Put "Mike" 51, Get "Mike", Return 5 ] -- und nu? Wie dem Ergebnis einen Namen geben?
-}

data DB a =
    Get String (Integer -> DB a)
  | Put String Integer (() -> DB a)
  | Return a

get :: String -> DB Integer
get key = Get key Return -- (\ value -> Return value)

put :: String -> Integer -> DB ()
put key value = Put key value Return

-- zwei DB-Programme verketten
splice :: DB a -> (a -> DB b) -> DB b
splice (Get key callback) next = undefined
splice (Put key value callback) next = undefined
splice (Return result) next = undefined


p1 = Put "Mike" 51 (\() ->
     Get "Mike" (\x ->
     Put "Mike" (x+1) (\() ->
     Get "Mike" (\y ->
     Return (x+y)))))



runDB :: DB a -> Map String Integer -> a
runDB (Get key callback) map = 
    let value = map ! key
    in runDB (callback value) map
runDB (Put key value callback) map = 
    let map' = Map.insert key value map
    in runDB (callback ()) map'
runDB (Return result) map = result