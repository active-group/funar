module Intro where

x :: Integer
x = 10

f :: Integer -> Integer
f x = x + 1

-- Ein Haustier ist eins der folgenden:
-- - Hund
-- - Katze
-- - Schlange
data Pet = Dog | Cat | Snake 
 deriving Show

-- Ist Haustier niedlich?
isCute :: Pet -> Bool 
isCute Dog = True
isCute Cat = True 
isCute Snake = False 

-- Gürteltier hat folgende Eigenschaften:
-- - lebendig oder tot
-- - Gewicht
data Liveness = Dead | Alive 
  deriving Show 

