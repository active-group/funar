module GameEvent where

import Cards

-- Was für Ereignisse passieren in meiner Domäne?
-- ---> Datentyp
-- "Ereignisse, die passiert sind" <- Vergangenheit

-- - Events müssen alles erzählen
-- - Redundanz ist OK
-- - fachlich motivieren

-- Event-Sourcing: Die Abfolge von Event-Objekten
-- enthält alle relevanten Daten der Domäne
-- vs. Data Warehouse

-- Commands
-- "Wunsch, daß etwas in der Zukunft passiert"
-- != Events

data GameEvent

data GameCommand

