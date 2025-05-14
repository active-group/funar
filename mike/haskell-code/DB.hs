module DB where

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