{-# LANGUAGE InstanceSigs #-}
module Intro where

x :: Integer
x = 43

y :: Integer
y = x * 2

-- >>> y
-- 84

-- Haustier ist eins der folgenden:
-- - Hund
-- - Katze
-- - Schlange
data Pet = Dog | Cat | Snake -- Aufzählung
  deriving Show