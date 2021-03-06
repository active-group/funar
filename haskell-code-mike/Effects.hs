{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs, FlexibleContexts, TypeOperators, DataKinds, PolyKinds #-}
{-# LANGUAGE LambdaCase, BlockArguments, TypeApplications, ScopedTypeVariables, ConstraintKinds #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -fplugin=Polysemy.Plugin #-}

module Effects where

import Polysemy
import Polysemy.Internal (send)
import qualified Polysemy.State as State 
import Polysemy.State (State)

import qualified Data.Map.Strict as Map 
import Data.Map.Strict (Map, (!))


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

{-
get :: Member DB effects => String -> Sem effects Integer
get key = send (Get key)

put :: Member DB effects => String -> Integer -> Sem effects ()
put key value = send (Put key value)
-}

-- TemplateHaskell, erzeugt den obigen Code
makeSem ''DB

--p1 :: Member DB effects => Sem effects String
-- '[]: Liste auf Typebene
p1 :: Sem '[DB] String
p1 = do put "Mike" 50
        x <- get "Mike"
        put "Mike" (x+1)
        y <- get "Mike"
        return ("Mike ist " ++ show x)

-- ': cons auf Typebene
runDBState :: Sem (DB ': effects) a -> Sem (State (Map String Integer) ': effects) a
runDBState =
    reinterpret (\ program ->
        case program of
            Get key -> do db <- State.get 
                          return (db ! key)
            Put key value -> do db <- State.get 
                                let db' = Map.insert key value db 
                                State.put db'
    )

runDBPure :: Map String Integer -> Sem (DB ': effects) a -> Sem effects (Map String Integer, a)
runDBPure db program = State.runState db (runDBState program)