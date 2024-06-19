module DB where

{-
put "Mike" 100
x = get "Mike"
put "Mike" (x+1)
y = get "Mike"
return (show (x + y))
-}

type Key = String

type Value = Integer

{- 
-- gescheitert:

data DBCommand a = -- a: Typ des Ergebnisses, von return
    Put Key Value
  | Get Key 
  | Return a
  deriving Show

type DBProgram a = [DBCommand a]

p1 = [
    Put "Mike" 100,
    Get "Mike",
    Put "Mike" ???
]

-}

