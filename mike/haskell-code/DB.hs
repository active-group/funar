module DB where


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
    Put Key Value (() -> DB a)
  | Get Key (Value -> DB a)
  | Return a