module Diagram where

import Data.Monoid
import Data.Semigroup

-- combine heißt jetzt <>
-- neutral heißt jetzt 

-- Größe mit inbegriffen + Position
data Prim = Square | Circle | Triangle | Smiley

-- Idee: primitive Bilder übereinanderlegen
-- oberstes Bild kommt zuerst
type Diagram = [Prim]

-- Brent Yorgey: Bilder müssen Monoid bilden

