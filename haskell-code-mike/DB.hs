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

-- "(Beschreibung eines) Datenbankprogramm mit Ergebnis vom Typ a"
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

-- >>> runDB Map.empty p1'
-- "103"
p1' :: DB String
p1' = splice (put "Mike" 51) (\() ->
      splice (get "Mike") (\x ->
      splice (put "Mike" (x+1)) (\() ->
      splice (get "Mike") (\y ->
      Return (show (x+y))))))

-- >>> runDB Map.empty p2'
-- 51
p2' :: DB Integer
p2' = splice (put "Mike" 51) (\() ->
              get "Mike")

p2'' = splice (Put "Mike" 51 Return) (\() ->
               Get "Mike" Return)

get :: String -> DB Integer
get key = Get key Return -- (\value -> Return value)

put :: String -> Integer -> DB ()
put key value = Put key value Return


splice :: DB a -> (a -> DB b) -> DB b
splice (Get key callback) next =
    Get key (\ value ->
        -- next value
        splice (callback value) next)
splice (Put key value callback) next =
    Put key value (\ () ->
        splice (callback ()) next)
splice (Return result) next = next result

-- Datenbankprogramm ausführen
-- "dependency injection"
runDB :: Map String Integer -> DB a -> a
-- >>> runDB Map.empty p1
-- Map.!: given key is not an element in the map
runDB mp (Get key callback) =
    let value = mp ! key
    in runDB mp (callback value)

runDB mp (Put key value callback) =
    let mp' = Map.insert key value mp
    in runDB mp' (callback ())

runDB mp (Return result) = result
