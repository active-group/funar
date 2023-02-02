{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE OverloadedStrings #-}
module DB where

import Data.Map (Map, (!))
import qualified Data.Map as Map

import Database.SQLite.Simple
import Database.SQLite.Simple.FromRow

-- Key-Value-Store: String -> Int


{-

  put "Johannes" 36
  x = get "Johannes"       oben: DB Int

  -- Int -> DB String
  put "Johannes" (x + 1)
  y = get "Johannes"
  return (show (x + y))      show ist toString()

-}


-- erster Versuch: Datentypen

-- data DBCommand a =
--     Put String Int
--   | Get String
--   | Return a

-- type DBProgram = [DBCommand]

-- p1 = [ Put "Johannes" 36
--      , Get "Johannes", ??????] <- wie geben wir dem Ergebnis einen Namen?


-- gescheitert :(

-- DB beschreibt ein Datenbankprogramm / einen Ablauf mit Ergebnis vom Typ a
data DB a =
    Get String (Int -> DB a)
  | Put String Int (() -> DB a)     -- () ist "Unit" (void)
  | Return a
--   deriving Show

p1 :: DB String
p1 = Put "Johannes" 36 (\ _ ->
       Get "Johannes" (\ x ->
         Put "Johannes" (x + 1) (\ _ ->
           Get "Johannes" (\ y ->
             Return (show (x + y))))))

-- einen einzelnen Wert aus der DB abrufen
get :: String -> DB Int
get key = Get key (\ value -> Return value)

getJohannes :: DB Int
getJohannes = get "Johannes"

-- einen einzelnen Wert in die DB schreiben
put :: String -> Int -> DB ()   -- hier kommt nichts Sinnvolles zurück -> Seiteneffekt
-- put key value = Put key value (\ result -> Return result)
put key value = Put key value Return

-- return :: a -> DB a
-- return = Return

-- put "Johannes" 36
-- x = get "Johannes"
-- put "Johannes" (x + 1)
-- ....

-- Datenbankprogramme "verbinden"/"aneinanderkleben"
-- foo :: DB a -> (a -> DB b) -> DB (a, b)   -- Tupel
splice :: DB a -> (a -> DB b) -> DB b   -- Tupel
splice (Get key callback) next =
    Get key (\ value -> splice (callback value) next)
splice (Put key value callback) next =
    Put key value (\ value -> splice (callback value) next)
splice (Return value) next = next value

-- \ value -> splice (callback value)
-- splice . callback     . ist o

-- p1' :: DB String
-- p1' = splice (put "Johannes" 36) (\ _ ->
--       splice (get "Johannes") (\ x ->
--       splice (put "Johannes" (x + 1)) (\ _ ->
--       splice (get "Johannes") (\ y ->
--       Return (show (x + y))))))

-- mit >>= statt splice
p1' :: DB String
p1' = put "Johannes" 36 >>= (\ _ ->
      getJohannes >>= (\ x ->
      put "Johannes" (x + 1) >>= (\ _ ->
      get "Johannes" >>= (\ y ->
      return (show (x + y))))))
    
-- erhöhe Alter und gib vorheriges zurück
increaseAge :: String -> DB Int
increaseAge name = do
    age <- get name
    put name (age + 1)
    return age

-- do-Notation
-- syntaktischer Zucker für genau p1'
p1'' :: DB String
p1'' = do
    put "Johannes" 36
    x <- increaseAge "Johannes"
    -- x <- get "Johannes"
    -- put "Johannes" (x + 1)
    y <- get "Johannes"
    return (show (x + y))

--          v  Typkonstruktor
-- class Monad m where
--     -- bind :: m a -> (a -> m b) -> m b
--     (>>=) :: m a -> (a -> m b) -> m b
--     return :: a -> m a

instance Functor DB where
    -- Übung

instance Applicative DB where

instance Monad DB where
    (>>=) :: DB a -> (a -> DB b) -> DB b
    (>>=) = splice

    return :: a -> DB a
    return = Return

-- wollen was "Echtes" machen mit DB!

-- Datenbankprogramm ausführen
-- "Echte Datenbank + Ablauf -> Ergebnis"
-- runDB :: Map String Int -> DB a -> a
runDB :: Map String Int -> DB a -> (a, Map String Int) -- Endzustand der "Datenbank" kommt mit zurück
runDB m (Get key callback) =
    let value = m ! key
     in runDB m (callback value)

runDB m (Put key value callback) =
    let newMap = Map.insert key value m
         --    nicht m!
     in runDB newMap (callback ())

runDB m (Return value) = (value, m)


-- mit der echten Welt reden!!!

--             v   IO-Monade
helloWorld :: IO ()
helloWorld = putStrLn "Hello world"

-- in IO geht do-Notation (weil es Monade ist)
greet :: IO ()
greet = do
    putStr "Wie heißt du? "
    name <- getLine
    putStrLn ("Hello " <> name)

runDBAsSQLite :: Connection -> DB a -> IO a
runDBAsSQLite conn (Get key callback) = do
    -- OverloadedStrings macht aus SQL-Text ein "Query"-Objekt
    [(MkEntry _ value)] <- 
        queryNamed conn "select key, value from entries where key = :key" [":key" := key]
    undefined

runDBAsSQLite conn (Put key value callback) = undefined

runDBAsSQLite conn (Return value) = return value -- a -> IO a