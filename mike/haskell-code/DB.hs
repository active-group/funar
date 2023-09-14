module DB where

import qualified Data.Map as Map
import Data.Map (Map, (!))

type Key = String
type Value = Integer

{-
   put "Mike" 100 
   x = get "Mike"
   put "Mike" (x + 1)
   y = get "Mike"
   return (show (x+y))
-}

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
    Put Key Value (()    -> DB a)
  | Get Key       (Value -> DB a)
  | Return a

put :: Key -> Value -> DB ()
put key value = Put key value (\() -> Return ())

get :: Key -> DB Value
get key = Get key (\value -> Return value)

splice :: DB a -> (a -> DB b) -> DB b
splice (Put key value callback) next =
    Put key value (\() -> splice (callback ()) next)
splice (Get key callback) next =
    Get key (\value -> splice (callback value) next)
splice (Return result) next = next result

p1 :: DB String
p1 = Put "Mike" 100 (\() ->
     Get "Mike" (\x ->
     Put "Mike" (x+1) (\() ->
     Get "Mike" (\y ->
     Return (show (x + y))))))

p1' = splice (put "Mike" 100) (\() ->
      splice (get "Mike") (\x ->
      splice (put "Mike" (x+1)) (\() ->
      splice (get "Mike") (\y ->
      Return (show (x+y))))))
    

runDB :: DB a -> Map Key Value -> (Map Key Value, a)
-- >>> runDB p1 Map.empty
-- (fromList [("Mike",101)],"201")
-- >>> runDB p1' Map.empty
-- (fromList [("Mike",101)],"201")
runDB (Put key value callback) mp =
    runDB (callback ()) (Map.insert key value mp)
runDB (Get key callback) mp =
    runDB (callback (mp ! key)) mp
runDB (Return result) mp = (mp, result)
