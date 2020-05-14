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
data DBCommand a =
    Put String Integer (() -> DBCommand a)
  | Get String (Integer -> DBCommand a) -- "Callback"
  | Done a

-- put :: String Integer DB -> DB

-- Lambda-Term:
-- \ x -> e
-- e1 e2 Beispiel: "f x"
-- x

-- \ x -> x x

p1 :: DBCommand String -- Datenbankprogramm, das eine Zeichenkette als Ergebnis liefert
p1 = Put "Mike" 15 (\() ->
     Get "Mike" (\x ->
     Put "Mike" (x + 1) (\() ->
     Done ("Mike ist " ++ show x))))

p1' = (put "Mike" 15) `splice` (\() ->
      (get "Mike") `splice` (\x ->
      (put "Mike" (x + 1)) `splice` (\() ->
      return' ("Mike ist " ++ show x))))

-- kann *nur* auf die Datenbank zugreifen
put :: String -> Integer -> DBCommand ()
put key value = Put key value Done

get :: String -> DBCommand Integer
get key = Get key Done

splice :: DBCommand a -> (a -> DBCommand b) -> DBCommand b
splice (Put key value cont) next =
  Put key value (\ () ->
    (cont ()) `splice` next)
splice (Get key cont) next =
  Get key (\ value ->
    (cont value) `splice` next)
splice (Done result) next = next result

return' x = Done x

evalDB :: Map String Integer -> DBCommand a -> a
evalDB db (Put key value cont) =
    evalDB (Map.insert key value db ) (cont ())
evalDB db (Get key cont) =
    case Map.lookup key db of
        Nothing -> undefined
        Just value ->
            evalDB db (cont value)
evalDB db (Done result) = result
