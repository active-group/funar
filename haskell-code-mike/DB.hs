module DB where

import Data.Map (Map)
import qualified Data.Map as Map 

{-
put "Mike" 50
x = get "Mike"
put "Mike" (x + 1)
y = get "Mike"
return ("Mike ist " ++ show x)
-}
{-
data DBCommand a =
    Put String Integer 
  | Get String
  | Return a

type DB a = [DBCommand a]

p1 = [
    Put "Mike" 50,
    Get "Mike", -- wie gebe ich dem Resultat einen Namen?
]
-}

-- Bei einer eingebetteten DSL, benutze für Variablen die
-- Variablen der "umgebenden Sprache" (Host-Sprache)

-- In einer FP-Sprache gibt es nur eine primitive Methode,
-- einen lokalen Namen einzuführen: Lambda

data DB a =
    Get String (Integer -> DB a) -- Callback / Continuation
  | Put String Integer (() -> DB a) 
  | Return a

-- Beschreibung eines DB-Programms mit Ergebnis vom Typ String
p1 :: DB String
p1 = Put "Mike" 50 (\() ->
     Get "Mike" (\x ->
     Put "Mike" (x + 1) (\() ->
     Get "Mike" (\y ->
     Return ("Mike ist " ++ show x)))))

get :: String -> DB Integer
get key = Get key Return -- (\value -> Return value)

put :: String -> Integer -> DB ()
put key value = Put key value Return -- (\() -> Return ())

splice :: DB a -> (a -> DB b) -> DB b
splice (Get key cont) next =
    Get key       (\value -> splice (cont value) next) 
splice (Put key value cont) next = 
    Put key value (\()    -> splice (cont ())    next)
splice (Return result) next = next result 

p1' :: DB String
p1' = put "Mike" 50 `splice` (\() ->
      get "Mike" `splice` (\x ->
      put "Mike" (x+1) `splice` (\() ->
      get "Mike" `splice` (\y ->
      Return ("Mike ist " ++ show x)))))

instance Functor DB where

instance Applicative DB where

instance Monad DB where
    (>>=) = splice
    return = Return

p1'' :: DB String
p1'' = do put "Mike" 50
          x <- get "Mike"
          put "Mike" (x+1)
          y <- get "Mike"
          return ("Mike ist " ++ show x)

-- maybeAdd :: Maybe Int -> Maybe Int -> Maybe Int
maybeAdd :: (Monad m, Num b) => m b -> m b -> m b
maybeAdd x y = 
    do xR <- x
       yR <- y
       return (xR + yR)

-- Aufgabe A:
-- instance Functor DB where 
--   fmap ... = ...

-- Aufgabe B:
-- Interpreter für DB
runDB :: Map String Integer -> DB a -> a
runDB = undefined

-- Aufgabe C:
-- State-Monade

-- Typ: State state a
-- produziert Ergebnis vom Typ a
-- führt dabei eine Zustandsvariable vom Type state mit

data State state a =
    Read (state -> State state a)
  | Write state (() -> State state a)
  | ReturnState a

{-
write :: state -> State state ()

read :: State state state

runState :: state -> State state a -> a
-}


-- Mehrere Effekte: historisch Monadentransformatoren
-- IRRWEG

-- stattdessen: freie Monade

--data DB' =
--    Get' String (Integer -> Free DB')

data DB' self =
    Get' String (Integer -> self)
  | Put' String Integer (() -> self)

-- brauchen einen Selbstbezug auf Free ops a
data Free ops a =
      Pure a -- "return"
    | Impure (ops (Free ops a))

type DB'' a = Free DB' a