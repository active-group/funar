module DB where

{-
put "Mike" 50
x = get "Mike"
put "Mike" (x+1)
y = get "Mike"
return (show (x + y))
-}

type Key = String
type Value = Integer

{-
data DBCommand result =
    Put Key Value
  | Get Key
  | Return result

type DBProgram result = [DBCommand result]

p1 = [Put "Mike" 50,
      Get "Mike"] -- ups
-}
data DB a =
    Get Key (Value -> DB a)
  | Put Key Value (() -> DB a)
  | Return a

p1 :: DB String
p1 = Put "Mike" 50 (\() ->
     Get "Mike" (\x -> 
     Put "Mike" (x+1) (\() ->
     Get "Mike" (\y ->
     Return (show (x+y))))))

-- fehlt noch:
-- 1. laufenlassen
-- 2. schick aussehen (ohne viele Klammern)