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
      Get "Mike",
      Put "Mike" (x+1)  -- failure!
-}

data DB a =
    Get Key       (Value -> DB a) -- trick, continuation
  | Put Key Value (()    -> DB a)
  | Return a

p1 :: DB String
p1 = Put "Mike" 100 (\() ->
     Get "Mike" (\x ->
     Put "Mike" (x+1) (\() ->
     Get "Mike" (\y ->
     Return (show (x+y))))))

runDB :: DB a -> Map Key Value -> (a, Map Key Value)

-- >>> runDB p1 Map.empty
-- ("201",fromList [("Mike",101)])

runDB (Get key cont) mp = 
    let value = mp ! key
    in runDB (cont value) mp
runDB (Put key value cont) mp =
    let mpNew = Map.insert key value mp
    in runDB (cont ()) mpNew
runDB (Return result) mp = (result, mp)
