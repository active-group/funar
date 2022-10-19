{-# LANGUAGE OverloadedStrings #-}
module DB where

import qualified Data.Map as Map -- alle Funktionen benutzen mit Map.
import Data.Map (Map, (!))

import Control.Applicative
import Database.SQLite.Simple
import Database.SQLite.Simple.FromRow

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

-- monadische Syntax
p1'' :: DB String
-- >>> runDB Map.empty p1''
-- "103"
p1'' = do put "Mike" 51
          x <- get "Mike"
          put "Mike" (x+1)
          y <- get "Mike"
          return (show (x+y))
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

{-
class Monad m where
  -- "bind" / "flatMap"
  (>>=) :: m a -> (a -> m b) -> m b
  return :: a -> m a
-}

-- optionalMap :: (a -> b) -> Optional a -> Optional b

-- >>> :type Return
-- Return :: a -> DB a
instance Functor DB where
    -- fmap ::        (a -> b)    -> DB a -> DB b
    -- flip splice :: (a -> DB b) -> DB a -> DB b
    fmap f (Get key callback) =
        Get key (\value -> fmap f (callback value))
    fmap f (Put key value callback) =
        Put key value (\() -> fmap f (callback ()))
    fmap f (Return result) = Return (f result)

instance Applicative DB where

instance Monad DB where
    (>>=) = splice
    return = Return

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

-- benötigen Datentyp, der den Datenbank-Zeilen entspricht
data Entry = MkEntry String Integer

instance FromRow Entry where
  fromRow = MkEntry <$> field <*> field

instance ToRow Entry where
    -- toRow :: Entry -> [SQLData]
    toRow (MkEntry key value) = toRow (key, value)


-- >>> x
-- 12
x =
    let [a, b] = [5,7]
    in a+b

runDBSQLite :: Connection -> DB a -> IO a
runDBSQLite conn (Get key callback) =
    do [(MkEntry _ value)] 
         -- OverloadedStrings macht aus SQL-Text ein Query-Objekt
         <- queryNamed conn "SELECT key, value FROM entries WHERE key = :key" [":key" := key]
       runDBSQLite conn (callback value)
runDBSQLite conn (Put key value callback) =
    do execute conn "REPLACE INTO entries (key, value) VALUES (?,?)" (MkEntry key value)
       runDBSQLite conn (callback ())
runDBSQLite conn (Return result) = return result

-- >>> execDB p1
-- "103"
execDB :: DB a -> IO a
execDB db =
    do conn <- open "test.db"
       execute_ conn
         "CREATE TABLE IF NOT EXISTS entries (key TEXT PRIMARY KEY, value INTEGER)"
       result <- runDBSQLite conn db
       close conn
       return result
