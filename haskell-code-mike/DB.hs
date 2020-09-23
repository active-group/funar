module DB where

import Data.Map.Strict as Map 
import Data.Map.Strict (Map, (!))

{-
  put "Mike" 105
  x = get "Mike"
  put "Frieda" (x + 1)
  return ("Mike ist " ++ (show x))

  Ein DB-Befehl ist eins der folgenden:
  - put
  - get
-}

{-
data DBCommand =
    Put String Integer
  | Get String

type DBProgram = [DBCommand]
-}

-- Datenbankprogramm mit Resultat vom Typ a
data DB a =
    Get String (Integer -> DB a)
  | Put String Integer (() -> DB a)
  | Done a -- Resultat

p1 = Put "Mike" 105 (\() ->
     Get "Mike" (\x ->
     Put "Frieda" (x + 1) (\() ->
     Done ("Mike ist " ++ (show x)))))

put :: String -> Integer -> DB ()
put key value = Put key value Done -- (\() -> Done ())

get :: String -> DB Integer
get key = Get key Done -- (\ value -> Done value)

p2' :: DB ()
p2' = put "Mike" 105

-- Zwei Programme hintereinanderschalten
splice :: DB a -> (a -> DB b) -> DB b 
splice (Get key cont) next = 
    Get key (\ value -> -- Integer 
               let x = cont value -- :: DB a
               in splice x next)
splice (Put key value cont) next = 
    Put key value (\ () ->
                    let x = cont ()
                    in splice x next)
splice (Done result) next = next result

p1' = put "Mike" 105 `splice`(\ () ->
      get "Mike" `splice` (\ x ->
      put "Frieda" (x + 1) `splice` (\() ->
      Done ("Mike ist " ++ (show x)))))
        
instance Functor DB where
    -- fmap :: (a -> b) -> DB a -> DB b
    fmap f (Get key cont) = Get key (\ value -> fmap f (cont value))
    fmap f (Put key value cont) = Put key value (\ value -> fmap f (cont ()))
    fmap f (Done result) = Done (f result)

instance Applicative DB where

instance Monad DB where
    return = Done
    (>>=) = splice

p1'' = do put "Mike" 105
          x <- get "Mike"
          put "Frieda" (x + 1)
          return ("Mike ist " ++ (show x))


run :: DB a -> Map String Integer -> a
run (Get key cont) db = 
    let value = db ! key
    in run (cont value) db
run (Put key value cont) db =
    let db' = Map.insert key value db
    in run (cont ()) db'
run (Done result) db = result