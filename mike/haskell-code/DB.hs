module DB where

{-
put "Mike" 51
x = get "Mike"
put "Mike" (x+1)
y = get "Mike"
return (show (x + y))
-}

type Key = String
type Value = Integer

{-
data DBCommand a = -- a: Typ des Ergebnisses 
    Put Key Value
  | Get Key
  | Return a

type DBProgram a = [DBCommand a]

p1 = [Put "Mike" 51,
      Get "Mike"]
-}

data DB a =
    Get Key (Value -> DB a)
  | Put Key Value (() -> DB a) -- (): "unit", Wert auch ()
  | Return a

p1 = Put "Mike" 51 (\() ->
     Get "Mike" (\x ->
     Put "Mike" (x+1) (\() ->
     Get "Mike" (\y ->
     Return (show (x+y))))))