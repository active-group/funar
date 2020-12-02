module GameEvent where

import Cards

-- Event: etwas, das in der Vergangenheit passiert ist
-- Die Events beschreiben, alles, was passiert ist.
-- Sie sollten aus der Domäne kommen.
-- Sie dürfen redundant sein.
data GameEvent =
      PlayerArrived Player
    | CardsDealt
    | GameStarted [Player]
    | CardPlayed Player Card
    | TrickDone Trick Player
    | InvalidMove Player Card
    | GameOver Player -- Gewinner


-- Command: Beschreibung von etwas, das passieren soll.
-- != Events