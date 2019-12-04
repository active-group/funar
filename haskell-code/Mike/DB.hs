module DB where

import qualified Data.Map as Map
import Data.Map (Map)

-- DB-DSL - Beispiele
-- put "Mike" 12
-- x = get "Mike"
-- put "Mike" (x + 1)
-- return (show x)

-- 1. Versuch:
{-
data DBCommand =
    Put String Int
  | Get String

put = Put
get = Get

[
    put "Mike" 12,
    get "Mike"
]
data DBProg = [DBCommand]
-}
-- (\ x -> x + 1) 5
-- let x = 5 in x + 1

data DBProg result =
      Get String     (Int -> DBProg result)
    | Put String Int (()  -> DBProg result)
            --       ^^^^^^^^^^^^^^^^^ Continuation
    | Return result

instance Functor DBProg where
    -- fmap :: (a -> b) -> DBProg a -> DBProg b
    fmap f (Get key cont) =
        Get key ( \ value ->
                  fmap f (cont value))
    fmap f (Put key value cont) =
        Put key value (\ () ->
                       fmap f (cont ()))
    fmap f (Return result) =
        Return (f result)

p1 = Put "Mike" 12 (\() ->
     Get "Mike" (\ x ->
     Put "Mike" (x + 1) (\ () ->
     Return (show x))))

put :: String -> Int -> DBProg ()
put key value = Put key value Return -- (\ () -> Return ())
get :: String -> DBProg Int
get key = Get key Return -- (\ result -> Return result)

ret result = Return result

c1 = put "Mike" 12
c2 = get "Mike"

-- an das DBProg a ein weiteres Programm anhängen
splice :: DBProg a -> (a -> DBProg b) -> DBProg b
splice (Get key cont) next =
    Get key (\ value ->
               splice (cont value) next)
splice (Put key value cont) next =
    Put key value (\ () ->
                     splice (cont ()) next)
splice (Return result) next = 
    next result

p2 = put "Mike" 12 `splice` (\ () ->
     get "Mike" `splice` (\ x ->
     put "Mike" (x + 1) `splice` (\ () ->
     ret (show x))))

instance Applicative DBProg where

instance Monad DBProg where
    (>>=) = splice
    return = ret

p3 = do put "Mike" 12
        x <- get "Mike"
        put "Mike" (x + 1)
        return (show x)

-- data Maybe a = Nothing | Just a
runDB :: Map String Int -> DBProg result -> result
runDB db (Get key cont) =
    let (Just value) = Map.lookup key db
    in runDB db (cont value)
runDB db (Put key value cont) =
    runDB (Map.insert key value db) (cont ())
runDB db (Return result) = result

r3 = runDB Map.empty p3