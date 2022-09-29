module DB where

import qualified Data.Map.Strict as Map
import Data.Map.Strict (Map, (!))

-- Abläufe beschreiben
-- Reihenfolge
-- Abhängigkeiten

{-
put "Mike" 51
x = get "Mike"
put "Mike" (x + 1)
y = get "Mike"
return (show (x + y))
-}

{-
-- Resultat vom Typ a
data DBCommand a =
    Put String Integer
  | Get String
  | Return a

type DBProgram a = [DBCommand a]

p1 = [Put "Mike" 51,
      Get "Mike",
      Put "Mike" ???]
-}

-- Idee, von FP-Seite: Benutze lambda, um dem Resultat einer Op einen Namen zu geben
-- Idee, NodeJS: Callback

data DB a =
    Get String (Integer -> DB a) -- <-- Continuation
  | Put String Integer (() -> DB a)
  | Return a

-- DB-Programm ausführen
runDB :: DB a -> Map String Integer -> a
runDB (Get key cont) map = 
    let value = map ! key
    in runDB (cont value) map
runDB (Put key value cont) map =
    let map' = Map.insert key value map
    in runDB (cont ()) map'
runDB (Return result) map = result

-- Beschreibung von etwas, was man mit der Datenbank machen könnte
p1 :: DB String
p1 = Put "Mike" 51 (\() ->
     Get "Mike" (\x ->
     Put "Mike" (x+1) (\() ->
     Get "Mike" (\y ->
     Return (show (x + y))))))

-- Die Klammern müssen noch weg.
-- Nix läuft.

