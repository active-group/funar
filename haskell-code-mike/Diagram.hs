{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
-- WICHTIG:
{-# LANGUAGE GeneralisedNewtypeDeriving #-}

module Diagram where

import Data.Monoid
import Data.Semigroup

-- combine heißt jetzt <>
-- neutral heißt jetzt 

-- Größe mit inbegriffen + Position
data Prim = Square | Circle | Triangle | Smiley
  deriving Show

-- Idee: primitive Bilder übereinanderlegen
-- oberstes Bild kommt zuletzt
-- type Diagram = [Prim]

-- Brent Yorgey: Bilder müssen Monoid bilden
-- Listen bilden einen Monoiden
-- hätte gern eine Monoid-Operation, beim dem das oberste Bild zuerst kommt

-- => Monoid Diagram, aber der falsche Monoid
-- [p1, p2, p3] <> [p4, p5] = [p1, p2, p3, p4, p5]

-- deswegen nächster Schritt:

{-
data Diagram = Diagram [Prim]

instance Semigroup Diagram where
    (Diagram prims1) <> (Diagram prims2) =  Diagram (prims2 ++ prims1)
-}

-- Geht auch einfacher:

newtype Diagram = Diagram (Dual [Prim])
  -- übernimmt Instanzen von Dual
  deriving (Semigroup, Monoid)

-- Strategie:
-- den Typ Diagram komplett mit schon vorhandenen
-- Monoid-Instanzen programmieren

