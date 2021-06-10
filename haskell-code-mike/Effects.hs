{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs, FlexibleContexts, TypeOperators, DataKinds, PolyKinds #-}
{-# LANGUAGE LambdaCase, BlockArguments, TypeApplications, ScopedTypeVariables, ConstraintKinds #-}
{-# OPTIONS_GHC -fplugin=Polysemy.Plugin #-}

module Effects where

import Polysemy
import Polysemy.Internal (send)

{-
data DB' self
  = Get' String (Integer -> self)
  | Put' String Integer (() -> self)

Get' :: String -> (Integer -> self) -> DB' self
Put' :: String -> Integer -> (() -> self) -> DB' self
-}


-- GADTs: Generalized Algebraic Datatypes

data DB monad a where -- nicht =, GADT
  Get :: String -> DB monad Integer  -- bei jedem Konstruktor wird a anders instanziert
  Put :: String -> Integer -> DB monad ()

-- Sem ist die freie Monade in Polysemy
-- effects ist eine Liste von Effekten

get :: Member DB effects => String -> Sem effects Integer
get key = send (Get key)

put :: Member DB effects => String -> Integer -> Sem effects ()
put key value = send (Put key value)

--p1 :: Member DB effects => Sem effects String
-- '[]: Liste auf Typebene
p1 :: Sem '[DB] String
p1 = do put "Mike" 50
        x <- get "Mike"
        put "Mike" (x+1)
        y <- get "Mike"
        return ("Mike ist " ++ show x)