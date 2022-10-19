module DB where

import qualified Data.Map as Map -- alle Funktionen benutzen mit Map.
import Data.Map (Map, (!))

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
    Get String         (Integer -> DB a)
  | Put String Integer (()      -> DB a)
  | Return a

p1 :: DB String
p1 = Put "Mike" 51 (\() ->
     Get "Mike" (\x ->
     Put "Mike" (x+1) (\() ->
     Get "Mike" (\y ->
     Return (show (x+y))))))

-- Datenbankprogramm ausführen
-- "dependency injection"
runDB :: Map String Integer -> DB a -> a
-- >>> runDB Map.empty p1
-- "103"
runDB mp (Get key callback) =
    runDB mp (callback (mp ! key))

runDB mp (Put key value callback) = 
    runDB (Map.insert key value mp) (callback ())

runDB mp (Return result) = result
