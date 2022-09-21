module DB where
{-
put "Mike" 51
x = get "Mike"
put "Mike" (x + 1)
y = get "Mike"
return x + y
-}

data DBCommand =
    Put String Integer
  | Get String
  | Return Integer

type DBProgram = [DBCommand]

