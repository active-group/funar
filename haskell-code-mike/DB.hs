module DB where

import qualified Data.Map as Map 
import Data.Map (Map)

{-
  put "Mike" 42
  x = get "Mike"
  put "Mike" (x + 1)
  done x
-}

{-
data DBCommand =
    Put String Integer
  | Get String

type DB = [DBCommand]

p1 = [Put "Mike" 42, ]

-}

-- Beschreibung einer Berechnung, welche ein Ergebnis vom Typ result
-- produziert und dabei auf eine Datenbank zugreift.
data DB result =
    Get String          (Integer -> DB result) -- Continuation
  | Put String Integer  (()      -> DB result)
  | Done result

data DBCommand self = -- Selbstbezug ... "DB result"
    Get' String         (Integer -> self)
  | Put' String Integer (() -> self)
-- kein Done!

-- freie Monade: Free f
data Free f result =
    Pure result
  | Impure (f (Free f result))
           -- ^^^^^^^^^^^^^^^ self, Selbstbezug

instance Functor f => Functor (Free f) where

instance Functor f => Applicative (Free f) where

instance Functor f => Monad (Free f) where
  return = Pure
  (Pure result) >>= next = next result
  (Impure effect) >>= next =
    Impure (fmap (>>= next) effect)

p1 = Put "Mike" 42 (\() ->
     Get "Mike" (\ x ->
     Put "Mike" (x + 42) (\() ->
     Done x)))

get :: String -> DB Integer 
get key = Get key Done -- (\ value -> Done value)

put :: String -> Integer -> DB ()
put key value = Put key value Done

p1' = put "Mike" 42 `splice` (\() ->
      get "Mike" `splice` (\ x ->
      put "Mike" (x + 42) `splice` (\() ->
      Done x)))

splice :: DB a ->           (a -> DB b) -> DB b 
splice (Get key cont)       next =
    Get key (\ value ->
        let dba = cont value -- :: DB a
        in splice dba next)
splice (Put key value cont) next =
    Put key value (\ () ->
        let dba = cont ()
        in splice dba next)
splice (Done result)        next = next result

compose :: (a -> DB b) -> (b -> DB c) -> (a -> DB c)
compose f g =
  \ a ->
    (f a) >>= g

{-
class Monad (m :: * -> *) where
  (>>=) :: m a -> (a -> m b) -> m b
  return :: a -> m a
-}

instance Functor DB where

instance Applicative DB where

instance Monad DB where
  (>>=) = splice  -- "bind/flatMap"
  return = Done

p1'' = put "Mike" 42 >>= (\() ->
       get "Mike" >>= (\ x ->
       put "Mike" (x + 42) >>= (\() ->
       return x)))

p1''' = do put "Mike" 42
           x <- get "Mike"
           put "Mike" (x + 42)
           return x



runDB :: Map String Integer -> DB a -> a
runDB db (Get key cont) =
    case Map.lookup key db of
        Nothing -> error "not in database"
        Just value -> runDB db (cont value)
runDB db (Put key value cont) =
    let db' = Map.insert key value db
    in runDB db' (cont ())
runDB db (Done result) = result
