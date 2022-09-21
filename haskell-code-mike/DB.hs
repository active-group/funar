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

p1 = Put "Mike" 51 (\() ->
     Get "Mike" (\x ->
     Put "Mike" (x+1) (\() ->
     Get "Mike" (\y ->
     Return (x+y)))))

runDB :: DB a -> Map String Integer -> a
runDB (Get key callback) map = 
    let value = map ! key
    in runDB (callback value) map
runDB (Put key value callback) map = undefined
runDB (Return result) map = result