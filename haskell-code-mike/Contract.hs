module Contract where

{-
Wie modellieren?

- Einfaches Beispiel?

(Banker: 1. Versuch Future)

Zero-Coupon Bond / Zero-Bond:

D11: Receive 100GBP on 29 Jan 2001

Immer implizit 2 Vertragspartner.

- Beispiel in kleinere Bausteine / "Ideen" zerlegen

3 Ideen:
- "später"
- Betrag / Vielfaches
- Währung
-}

type Amount = Double

data Currency = EUR | GBP | CHF

data Date = Date String -- ISO-Notation
  deriving (Ord, Eq)

{-
data Contract =
    ZeroCouponBond Amount Currency Date
  | Future
  | Annapurna
  | Himalaya
  | K2

{- 
Probleme:
- Muß immer erweitert werden
- Jeder neue Vertrag muß neu interpretiert werden
=> aufwendig & fehleranfällig
-}

-}