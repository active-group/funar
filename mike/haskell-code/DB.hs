module DB where

{-
put "Mike" 100
x = get "Mike"
put "Mike" (x+1)
y = get "Mike"
return (show (x + y))
-}

type Key = String

type Value = Integer

{- 
-- gescheitert:

data DBCommand a = -- a: Typ des Ergebnisses, von return
    Put Key Value
  | Get Key 
  | Return a
  deriving Show

type DBProgram a = [DBCommand a]

p1 = [
    Put "Mike" 100,
    Get "Mike",
    Put "Mike" ???
]
-}

-- Namen vergeben: let, =, \

data DB a =
    Get Key       (Value -> DB a) -- callback / continuation
  | Put Key Value (()    -> DB a)
  | Return a

p1 = Put "Mike" 100 (\() ->
     Get "Mike" (\x -> 
     Put "Mike" (x+1) (\() ->
     Get "Mike" (\y ->
     Return (show (x+y))))))