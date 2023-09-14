module DB where

import qualified Data.Map as Map
import Data.Map (Map)

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

p1 :: DB String
p1 = Put "Mike" 100 (\() ->
     Get "Mike" (\x ->
     Put "Mike" (x+1) (\() ->
     Get "Mike" (\y ->
     Return (show (x + y))))))

runDB :: DB a -> Map Key Value -> (Map Key Value, a)
runDB (Put key value callback) mp = undefined
runDB (Get key callback) mp =
    key (callback 
runDB (Return result) mp = (mp, result)
