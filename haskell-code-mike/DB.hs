module DB where

import Data.Map

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

data DB a =
    Get String         (Integer -> DB a) -- continuation
  | Put String Integer (()      -> DB a)
  | Return a

get :: String -> DB Integer
-- get key = Get key (\ value -> Return value)
get key = Get key Return

put :: String -> Integer -> DB ()
put key value = Put key value  (\ () -> Return ())

p1 :: DB Integer
p1 = Put "Mike" 50 (\() ->
     Get "Mike" (\ x ->
     Put "Mike" (x + 1) (\ () ->
     Get "Mike" (\ y ->
     Return (x + y)))))

-- Interpreter
runDB :: DB a -> Map String Integer -> a
runDB (Get key cont) mp = 
    runDB (cont (mp ! key)) mp
runDB (Put key value cont) mp = 
    runDB (cont ()) (insert key value mp)
runDB (Return result) mp = result