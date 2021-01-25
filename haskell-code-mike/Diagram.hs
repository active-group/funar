module Diagram where

import Data.Monoid
import Data.Semigroup

-- combine heißt jetzt <>
-- neutral heißt jetzt 

data Prim = Square | Circle | Triangle | Smiley

type Diagram = [Prim]