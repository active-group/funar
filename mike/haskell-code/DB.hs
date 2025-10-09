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
data DBCommand result =
    Put Key Value
  | Get Key
  | Return result

type DBProgram result = [DBCommand result]

p1 = [Put "Mike" 100,
      Get "Mike",
      Put "Mike" (x+1)] -- gibt keine Namen :-(
-}

-- >>> :type ()
-- () :: ()

data DB a =  -- fka result
    Get Key       (Value -> DB a) -- callback
  | Put Key Value (() -> DB a) 
  | Return a

p1 = Put "Mike" 100 (\() ->
     Get "Mike" (\x -> 
     Put "Mike" (x+1) (\() ->
     Get "Mike" (\y -> 
     Return (show (x+y))))))

-- notationelle Annäherung
get :: Key -> DB Value
get key = Get key Return

put :: Key -> Value -> DB ()
put key value = Put key value Return -- (\() -> Return ())

-- >>> runDB p1 Map.empty
-- ("201",fromList [("Mike",101)])

-- "dependency injection"
runDB :: DB a -> Map Key Value -> (a, Map Key Value)
runDB (Get key callback) mp =
    let value = mp ! key
    in runDB (callback value) mp
runDB (Put key value callback) mp = 
    let mp' = Map.insert key value mp
    in runDB (callback ()) mp'
runDB (Return result) mp = (result, mp)
