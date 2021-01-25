module Validation where

import Data.Char

data Person = Person {
    personName :: String,
    personEmail :: String,
    personAge :: Int
}

p1 = Person "Mike" "sperber@deinprogramm.de" 49
p2 = Person "Fritz" "fritz/-/(&/(" 500


-- Typsystem nicht stark genug, um illegale Daten zu verbieten

-- Funktionale Programmierung: "make illegal states unrepresentable"
-- schwache Version: "don't create illegal states"

-- "parse, don't validate"

{-
-- Fehlschlag 0:
validateAge age = if age <= 120
                  then Just age
                  else Nothing -- keine Fehlermeldung

-- Fehlschlag 1: (kein Parsing)
validateAge' age = if age <= 120
                   then Nothing -- kein Parsing
                   else Just "too old"
-}

data Validation error result =
    Success result
  | Failure [error]

validateAge age = if age <= 120
                  then Success age
                  else Failure ["too old"]

validateName name = Success name

validateEmail email = if elem '@' email 
                      then Success (map toLower email)
                      else Failure ["no at sign"]
