module DB where

import qualified Data.Map.Strict as Map
import Data.Map.Strict (Map)

type Key = String
type Value = Integer

{-
put "Mike" 100
x = get "Mike" 
put "Mike" (x+1)
y = get "Mike"
return (show (x+y))
-}

{-
data DBCommand result =
    Put Key Value
  | Get Key
  | Return result

type DBProgram result = [DBCommand result]

p1 = [Put "Mike" 100,
      Get "Mike",
      Put "Mike"] -- wie dem Resultat vom Get einen Namen geben?
-}

data DB a =
    Get Key       (Value -> DB a)
  | Put Key Value (()    -> DB a)
  | Return a

p1 :: DB String
p1 =
    Put "Mike" 100 (\() ->
    Get "Mike" (\x ->
    Put "Mike" (x+1) (\() ->
    Get "Mike" (\y -> 
    Return (show (x+y))))))

runDB :: DB a -> Map Key Value -> a
runDB (Get key callback) = undefined
runDB (Put key value callback) = undefined
runDB (Return result) = undefined