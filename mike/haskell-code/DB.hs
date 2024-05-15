module DB where

{-
put "Mike" 10
x = get "Mike"
put "Mike" (x * 2)
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

p1 = [Put "Mike" 10,
      x = Get "Mike",
      Put "Mike" (x+ * 2)]
-}

data DB a =
    Get Key       (Integer -> DB a)
  | Put Key Value (()      -> DB a)
  | Return a

p1 = Put "Mike" 10 (\() ->
     Get "Mike" (\x -> 
     Put "Mike" (x * 2) (\() ->
     Get "Mike" (\y ->
     Return (show (x+y))))))