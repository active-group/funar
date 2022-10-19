module DB where

{-
  put "Mike" 51
  x = get "Mike"
  put "Mike" (x+1)
  y = get "Mike"
  return (show (x+y))
-}

{-
data DBCommand a =
    Put String Integer
  | Get String
  | Return a

type DBProgram a = [DBCommand a]

p1 = [Put "Mike" 51,
      Get "Mike"] -- wie dem Ergebnis einen Namen geben?

-}
data DB a =
    Get String (Integer -> DB a)
  | Put String Integer (()      -> DB a)
  | Return a

p1 :: DB String
p1 = Put "Mike" 51 (\() ->
     Get "Mike" (\x ->
     Put "Mike" (x+1) (\() ->
     Get "Mike" (\y ->
     Return (show (x+y))))))
