module DB where

import qualified Data.Map as Map
import Data.Map (Map)

{-
put "Mike" 48
x <- get "Mike"
put "Mike" (x + 1)
return (show (x * 2))
-}

-- Programm als Wert
-- 1. Entwurf
{-
data DBProgram = [DBCommand]
data DBCommand =
     Put String Exp
   | Bind String DBCommand
   | Get String
data Exp = Value Integer | Variable String
put = Put
get = Get
(:=) = Bind

p1 = [put "Mike" 48,
      x := Get "Mike",
      Bind "x" (get "Mike"), 
      put "Mike" (Variable "x" + 1)]
-}

-- Der Mechanismus um Dingen einen Namen zu geben,
-- ist Lambda

-- 2. Versuch
data DBProg a =
    Get String (Integer -> DBProg a) -- <- sagt, wie's weitergeht
  | Put String Integer (DBProg a) -- "Rest"
  | Done a
-- Funktion, die sagt, wie's weitergeht:
-- Continuation
p1 = Put "Mike" 48
       (Get "Mike"
         (\ x ->
            Put "Mike" (x + 1) 
              (Done (show (x * 2)))))

instance Functor DBProg where
    fmap f (Get key fnext) = 
        Get key (\ result -> -- result :: Integer
          fmap f (fnext result)) -- fnext result :: DBProg a
    fmap f (Put key value next) =
        Put key value (fmap f next) -- :: DBProg b
    fmap f (Done result) = Done (f result)

runDB :: (DBProg a) -> (Map String Integer) -> a
runDB (Get key fnext) db =
    let (Just result) = Map.lookup key db
    in runDB (fnext result) db
runDB (Put key value next) db  =
    let db' = Map.insert key value db
    in runDB next db'
runDB (Done result) db   = result

-- Wunsch

get :: String -> DBProg Integer
get key = Get key Done -- (\ result -> Done result)

put :: String -> Integer -> DBProg ()
put key value = Put key value (Done ())

-- zwei Programme aneinanderspleißen
splice :: DBProg a -> (a -> DBProg b) -> DBProg b
splice (Put key value next) f =
    Put key value
      (splice next f)
splice (Get key fnext)      f =
    Get key (\ result ->
        let dba = fnext result -- DBProg a
        in splice dba f)
splice (Done result)        f = f result

p1' =
  let m = "Mike"
  in
    put m 48 `splice` (\ () -> 
    get m `splice` (\ x ->
    put m (x + 1) `splice` (\ () ->
    Done (show (x * 2)))))

instance Monad DBProg where
    (>>=) = splice -- "bind", Scala, Java: "flatMap"
    return = Done

instance Applicative DBProg where
    pure = Done
    dbf <*> dba = undefined

p1'' =
 do put "Mike" 48
    x <- get "Mike"
    put "Mike" (x + 1)
    return (show (x * 2))

