module DB where

import qualified Data.Map as Map 
import Data.Map (Map)

-- getDB :: key -> value

-- DSL für Datenbankzugriffe

-- put "Mike" 15
-- x = get "Mike"
-- put "Mike" (x + 1)
-- return ("Mike ist " ++ show x)

{-
Erster Versuch:

type DB = [DBCommand]

data DBCommand =
    Put String Integer
  | Get String
-}

-- Datenbankprogramm mit Resultat vom Typ a
data DB a =
    Put String Integer (() -> DB a)
  | Get String (Integer -> DB a) -- "Callback"
  | Done a

p1 :: DB String -- Datenbankprogramm, das eine Zeichenkette als Ergebnis liefert
p1 = Put "Mike" 15 (\() ->
     Get "Mike" (\x ->
     Put "Mike" (x + 1) (\() ->
     Done ("Mike ist " ++ show x))))

p1' = put "Mike" 15 `splice` (\() ->
      get "Mike" `splice` (\x ->
      put "Mike" (x + 1) `splice` (\() ->
      return' ("Mike ist " ++ show x))))

put :: String -> Integer -> DB ()
put key value = Put key value Done

get :: String -> DB Integer
get key = Get key Done

splice :: DB a -> (a -> DB b) -> DB b
splice = undefined

return' x = Done x

evalDB :: Map String Integer -> DB a -> a
evalDB db (Put key value cont) =
    evalDB (Map.insert key value db ) (cont ())
evalDB db (Get key cont) =
    case Map.lookup key db of
        Nothing -> undefined
        Just value ->
            evalDB db (cont value)
evalDB db (Done result) = result
