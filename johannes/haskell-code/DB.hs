module DB where

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

-- einen einzelnen Wert in die DB schreiben
put :: String -> Int -> DB ()   -- hier kommt nichts Sinnvolles zurück -> Seiteneffekt
put key value = Put key value (\ result -> Return result)

-- return :: a -> DB a
-- return = Return

-- put "Johannes" 36
-- x = get "Johannes"
-- put "Johannes" (x + 1)
-- ....

-- Datenbankprogramme "verbinden"/"aneinanderkleben"
-- foo :: DB a -> (a -> DB b) -> DB (a, b)   -- Tupel
splice :: DB a -> (a -> DB b) -> DB b   -- Tupel
splice (Get key callback) next = undefined
splice (Put key value callback) next = undefined
splice (Return value) next = next value
