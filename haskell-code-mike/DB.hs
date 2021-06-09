module DB where

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
      put "Mike" `splice` (\() ->
      get "Mike" `splice` (\y ->
      Return ("Mike ist " ++ show x)

