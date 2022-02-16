{-# LANGUAGE OverloadedStrings #-}

module DB where

import qualified Data.Map.Strict as Map 
import Data.Map (Map, (!))

import Database.SQLite.Simple
import Database.SQLite.Simple.FromRow

{-
put "Mike" 50
x = get "Mike"
put "Mike" (x+1)
y = get "Mike"
return (show (x + y))
-}

{-
data DBCommand a =
    Put String Integer 
  | Get String 
  | Return a

type DBProgram a = [DBCommand a]

p1 = [Put "Mike" 50, 
      Get "Mike"]
-}

data DB a =
    Get String         (Integer -> DB a) -- "callback" / "continuation"
  | Put String Integer (()      -> DB a)
  | Return a

p1 :: DB String
p1 = Put "Mike" 50 (\() ->
     Get "Mike" (\x ->
     Put "Mike" (x+1) (\() ->
     Get "Mike" (\y ->
     Return (show (x + y))))))

get :: String -> DB Integer 
get key = Get key Return -- (\ value -> Return value)

put :: String -> Integer -> DB ()
put key value = Put key value Return -- (\() -> Return ())

splice :: DB a -> (a -> DB b) -> DB b
splice (Get key cont) next =
    Get key (\ value ->
               splice (cont value) next)
splice (Put key value cont) next =
    Put key value (\ () ->
                    splice (cont ()) next)
splice (Return result) next = next result

p1' :: DB String
p1' = put "Mike" 50 `splice` (\() ->
      get "Mike" `splice` (\x -> 
      put "Mike" (x+1) `splice` (\() ->
      get "Mike" `splice` (\y ->
      Return (show (x+y))))))

p1''' :: DB String
p1''' =
  put "Mike" 50 >>= ( \() ->
  get "Mike">>= ( \x ->
  put "Mike" (x + 1) >>= ( \() ->
  get "Mike" >>= ( \y ->
  return (show (x + y))))))
-- >>> runDB p1 Map.empty
-- "101"
-- >>> runDB p1' Map.empty
-- "101"
-- >>> runDB p1'' Map.empty
-- "101"
runDB :: DB a -> Map String Integer -> a
runDB (Get key cont) db = 
    runDB (cont (db ! key)) db
runDB (Put key value cont) db = 
    runDB (cont ()) (Map.insert key value db)
runDB (Return result) db = result

data Entry = Entry String Integer 

instance FromRow Entry where
  fromRow = (fmap Entry field) <*> field

instance ToRow Entry where
    toRow (Entry key value) = toRow (key, value)

runDBSQLite :: DB a -> Connection -> IO a
runDBSQLite (Get key cont) connection = 
  do [Entry _ value] <-
       (queryNamed connection "SELECT * from test WHERE key = :key" [":key" := key] :: IO [Entry])
     runDBSQLite (cont value) connection
runDBSQLite (Put key value cont) connection =
  do execute connection "INSERT IF NOT EXISTS INTO test (key, value) VALUES (?, ?)" (Entry key value)
     runDBSQLite (cont ()) connection
runDBSQLite (Return result) connection = return result

-- >>> execDB p1
-- <BLANKLINE>
-- ByteCodeLink.lookupCE
-- During interactive linking, GHCi couldn't find the following symbol:
--   sqlitezmsimplezm0zi4zi18zi0zmad88ef380b37fb3f5c27133eac3f56151f083f2bd11a58038bb3dbe1bf7f84bd_DatabaseziSQLiteziSimpleziFromField_zdfFromFieldInteger_closure
-- This may be due to you not asking GHCi to load extra object files,
-- archives or DLLs needed by your current session.  Restart GHCi, specifying
-- the missing library using the -L/path/to/object/dir and -lmissinglibname
-- flags, or simply by naming the relevant files on the GHCi command line.
-- Alternatively, this link failure might indicate a bug in GHCi.
-- If you suspect the latter, please report this as a GHC bug:
--   https://www.haskell.org/ghc/reportabug
execDB :: DB a -> IO a
execDB command =
  do connection <- open "test.db"
     execute_ connection "CREATE TABLE IF NOT EXISTS test (key TEXT PRIMARY KEY, value INTEGER)"
     result <- runDBSQLite command connection 
     close connection
     return result


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
          return (show (x+y))

-- IO ... 
