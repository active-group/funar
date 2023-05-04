module DB where

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
    Get Key (Value -> DB a) -- Callback / Continuation
  | Put Key Value (() -> DB a)
  | Return a
