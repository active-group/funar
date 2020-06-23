module Intro where

data State =
  Solid | Liquid | Gas
  deriving (Show, Eq) -- generiert Typklassen-Instanzen für Show, Eq
