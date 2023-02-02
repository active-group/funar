module DB where

-- Key-Value-Store: String -> Int


{-

  put "Johannes" 36
  x = get "Johannes"
  put "Johannes" (x + 1)
  y = get "Johannes"
  return (show (x + y))      show ist toString()

-}


-- erster Versuch: Datentypen

data DBCommand a =
    Put String Int
  | Get String
  | Return a

type DBProgram = [DBCommand]

p1 = [ Put "Johannes" 36
     , Get "Johannes", ??????]