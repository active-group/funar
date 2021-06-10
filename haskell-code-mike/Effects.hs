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

get :: Member DB effects => String -> Sem effects Integer
get key = send (Get key)