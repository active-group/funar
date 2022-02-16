module DB where

{-
put "Mike" 50
x = get "Mike"
put "Mike" (x+1)
y = get "Mike"
return (show (x + y))
-}

{-
data DBCommand a =
    Put String Integer 
  | Get String 
  | Return a

type DBProgram a = [DBCommand a]

p1 = [Put "Mike" 50, 
      Get "Mike"]
-}

data DB a =
    Get String (Integer -> DB a) -- "callback" / "continuation"