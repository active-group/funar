module DB where
{-
put "Mike" 51
x = get "Mike"
put "Mike" (x + 1)
y = get "Mike"
return x + y
-}

data DBCommand a =
    Put String Integer
  | Get String
  | Return a

type DBProgram a = [DBCommand a]

p1 :: DBProgram Integer
p1 = [Put "Mike" 51, Get "Mike", Return 5 ] -- und nu? Wie dem Ergebnis einen Namen geben?



