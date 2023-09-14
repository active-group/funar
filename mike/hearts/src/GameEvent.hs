module GameEvent where

import Cards

import qualified Data.Map as Map
import Data.Map (Map)

-- Event ist eine Repräsentation eines Ereignisses in unserer Domäne.

-- - Event ist in der Vergangenheit passiert.
-- - Event bezieht sich auf die Domäne, nicht die Technik.
-- - Events erzählen die vollständige Geschichte der Domäne.
-- - Redundanz ist OK.
data GameEvent = 
    DealHands Player Hand
  | PlayCard Player Card
  | PlayerStartsGame Player
  | AllPlayersHavePlayed Trick
  | PlayerTookTrick Player Trick
  