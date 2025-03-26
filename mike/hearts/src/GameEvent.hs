module GameEvent where

import Cards

import qualified Data.Map as Map
import Data.Map (Map)

{-
Event: Datenwert, der ein Ereignis beschreibt
- ist passiert - in der Vergangenheit
- alle Informationen über das Ereignis
- Redundanz OK
- Zustand nicht OK
- fachlich, nicht technisch

Commands: Datenwert, der einen Wunsch beschreibt, daß etwas passiert
- in der Zukunft, noch nicht passiert
-}