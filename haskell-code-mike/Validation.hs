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

-- Problem: Komposition

{-
class Functor f where
    fmap :: (a -> b) -> f a -> f b
-}

-- akzeptiert noch Typvariable result
instance Functor (Validation error) where
    fmap f 

{-
validatePerson :: String -> String -> Int -> Validation String Person
validatePerson name email age =
    case validateName name of
        Success name ->
            case validateEmail email of
                Success email ->
                    case validateAge age of
                        Success age ->
                            Success (Person name email age)
                        Failure errors -> Failure errors
                Failure errors ->
                    case validateAge age of
                        Success age -> Failure errors
                        Failure errors' -> Failure (errors ++ errors')
 -- ...

 data PersonDraft = PersonDraft {
    personDraftName :: Validation String String,


-}