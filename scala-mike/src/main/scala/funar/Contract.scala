package funar

import javax.swing.plaf.BorderUIResource.EmptyBorderUIResource

/*
Finanzderivate:

Zins-Swap
in der Zukunft:
bekomme festen Zinssatz
zahle variablen Zinssatz

2 Vertragspartner:innen

1. finde ein einfaches Beispiel

Rollover-Geschäft: heute was bekommen, morgen was mit Zinsen zahlen

- heute was bekommen
- morgen was mit Zinsen zahlen
- morgen was (ohne Zinsen) zahlen
- morgen was (ohne Zinsen) bekommen

"Am 24.12. bekomme ich 100€": Zero-Bond, Zero-Coupon Bond

3 Ideen:

- € "Währung"
- Vielfaches
- Später
*/

case class Date(iso: String)

type Amount = Double

enum Currency {
  case EUR 
  case GBP
  case USD
}

/*
enum Contract {
  case ZeroCouponBond(date: Date, amount: Amount, currency: Currency)
}
*/

object Contract {
  import Currency._
  import Contract._
  // val zcb1 = ZeroCouponBond(Date("2022-12-24"), 100, EUR)
}