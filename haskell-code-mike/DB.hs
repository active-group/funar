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

p1 :: DB Integer
p1 = Put "Mike" 50 (\() ->
     Get "Mike" (\ x ->
     Put "Mike" (x + 1) (\ () ->
     Get "Mike" (\ y ->
     Return (x + y)))))

runDB :: DB a -> Map String Integer -> a