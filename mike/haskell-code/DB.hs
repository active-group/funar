{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE OverloadedStrings #-}
module DB where

import qualified Data.Map.Strict as Map
import Data.Map.Strict (Map, (!))

import Control.Applicative
import Database.SQLite.Simple
import Database.SQLite.Simple.FromRow

{-
   put "Mike" 100
   x = get "Mike"
   put "Mike" (x+1)
   y = get "Mike"
   return (show (x+y))

-}

type Key = String
type Value = Integer

{-
data DBCommand result =
    Put Key Value
  | Get Key
  | Return result

type DBProgram result = [DBCommand result]

p1 = [Put "Mike" 100,
      Get "Mike",
      Put "Mike" (x+1)] -- gibt keine Namen :-(
-}

-- >>> :type ()
-- () :: ()

data DB a =  -- fka result
    Get Key       (Value -> DB a) -- callback
  | Put Key Value (() -> DB a) 
  | Return a

p1 = Put "Mike" 100 (\() ->
     Get "Mike" (\x -> 
     Put "Mike" (x+1) (\() ->
     Get "Mike" (\y -> 
     Return (show (x+y))))))

-- notationelle Annäherung
get :: Key -> DB Value
get key = Get key Return

put :: Key -> Value -> DB ()
put key value = Put key value Return -- (\() -> Return ())

splice :: DB a -> (a -> DB b) -> DB b
splice (Get key callback) next =
    Get key (\ value ->
               splice (callback value) next)
splice (Put key value callback) next =
    Put key value (\ () ->
                   splice (callback ()) next)
splice (Return result) next = next result

-- >>> runDB p1' Map.empty 
-- ("201",fromList [("Mike",101)])

p1' :: DB String
p1' = splice (put "Mike" 100) (\() ->
      splice (get "Mike") (\x ->
      splice (put "Mike" (x+1)) (\() ->
      splice (get "Mike") (\y ->
      Return (show(x+y))))))

-- >>> :info Monad
-- type Monad :: (* -> *) -> Constraint
-- class Applicative m => Monad m where
--   (>>=) :: m a -> (a -> m b) -> m b
--   return :: a ->

instance Functor DB where

instance Applicative DB where

instance Monad DB where
    -- "bind"
    (>>=) :: DB a -> (a -> DB b) -> DB b
    (>>=) = splice
    return :: a -> DB a
    return = Return

-- >>> runDB p1'' Map.empty
-- ("201",fromList [("Mike",101)])
p1'' :: DB String
p1'' = do put "Mike" 100
          x <- get "Mike"
          put "Mike" (x+1)
          y <- get "Mike"
          return (show (x+y))

-- >>> runDB p1 Map.empty
-- ("201",fromList [("Mike",101)])

-- "dependency injection"
runDB :: DB a -> Map Key Value -> (a, Map Key Value)
runDB (Get key callback) mp =
    let value = mp ! key
    in runDB (callback value) mp
runDB (Put key value callback) mp = 
    let mp' = Map.insert key value mp
    in runDB (callback ()) mp'
runDB (Return result) mp = (result, mp)

data Entry = MkEntry Key Value

instance FromRow Entry where
    fromRow :: RowParser Entry
    fromRow = MkEntry <$> field <*> field

instance ToRow Entry where
    toRow (MkEntry key value) = toRow (key, value)

runDBSQLite :: DB a -> Connection -> IO a
runDBSQLite (Get key callback) c =
    do [entry] <- queryNamed c "SELECT key, value FROM entries WHERE key = :key" [":key" := key]
       let (MkEntry _key value) = entry
       runDBSQLite (callback value) c
runDBSQLite (Put key value callback) c =
    do execute c "REPLACE INTO entries (key, value) VALUES (?, ?)" (MkEntry key value)
       runDBSQLite (callback ()) c
runDBSQLite (Return result) c = return result

execDB :: DB a -> IO a
execDB program =
    do c <- open "test.db"
       execute_ c
         "CREATE TABLE IF NOT EXISTS entries (key TEXT PRIMARY KEY, value INTEGER)"
       result <- runDBSQLite program c
       close c
       return result