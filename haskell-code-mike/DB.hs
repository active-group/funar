module DB where

{-
put "Mike" 50
x = get "Mike"
put "Mike" (x + 1)
y = get "Mike"
return (x + y)
-}
{-
data DBCommand =
    Put String Integer
  | Get String 

type DBProgram = [DBCommand]

p1 :: [DBCommand]
p1 = [Put "Mike" 50, Get "Mike" ] -- können Ergebnis keinen Namen geben: fail
-}

data DB =
    Get String (Integer -> DB)
  | Put String Integer (() -> DB)