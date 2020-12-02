module DB where

{-
Wunsch:

put "Mike" 15
x = get "Mike"
put "Mike" (x + 1)
y = get "Mike"
done y -- return
-}

{-
-- 1. Versuch
data DBCommand =
    Put String Int
  | Get String

type DB = [DBCommand]

put key value = Put key value
get key = Get key


p1 = [put "Mike" 15,
      get "Mike",
      put "Mike" ] -- Mist
-}

-- \ x -> put "Mike" (x + 1)

data DB result =
    Put String Int (() -> DB result)
  | Get String (Int -> DB result) -- Callback
  | Done result

p1 :: DB Int
p1 = Put "Mike" 15 (\ () ->
     Get "Mike" (\ x ->
     Put "Mike" (x + 1) (\ () ->
     Get "Mike" (\ y ->
     Done y))))

put :: String -> Int -> DB ()
put key value = Put key value (\ () -> Done ())

c1 = put "Mike" 15

get :: String -> DB Int
get key = Get key Done -- (\ value -> Done value)
