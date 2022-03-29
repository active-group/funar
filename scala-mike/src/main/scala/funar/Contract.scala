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

2. einfache Beispiel in "atomare Bestandteile" zerlegt

3 Ideen:

- € "Währung"
- Vielfaches
- Später

3. finde Selbstbezüge / Kombinatoren

4. sukzessive größere Beispiele abbilden, ggf. neue Kombinatoren definieren
*/

case class Date(iso: String) {
  def before(other: Date): Boolean = this.iso < other.iso
}

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

enum Contract {
  case One(currency: Currency)
  // case Multiple(amount: Amount, currency: Currency)
  case Multiple(amount: Amount, contract: Contract)
  case Forward(date: Date, contract: Contract)
  case Reverse(contract: Contract)
  case Combined(contract1: Contract, contract2: Contract)
  case Zero
}

object Contract {
  import Currency._
  import Contract._
  val zcb1 = Forward(Date("2022-12-24"), Multiple(100, One(EUR)))

  def zeroCouponBond(date: Date, amount: Amount, currency: Currency): Contract =
    Forward(date, Multiple(amount, One(currency)))

  val zcb1_ = zeroCouponBond(Date("2022-12-24"), 100, EUR)

  val rollover = Combined(zeroCouponBond(Date("2022-03-29"), 100, EUR),
                          Reverse(zeroCouponBond(Date("2022-03-30"), 105, EUR)))


  enum Direction {
    case Long 
    case Short

    def invert = this match {
      case Long => Short
      case Short => Long
    }
  }

  case class Payment(date: Date, direction: Direction, amount: Amount, currency: Currency) {
    def invert = this.copy(direction = this.direction.invert)
    def scale(factor: Double) = this.copy(amount = this.amount * factor)
  }

  // alle Zahlungen, die bis heute fällig sind
  // heraus kommt ein "Residualvertrag"
  def meaning(contract: Contract, today: Date): (Seq[Payment], Contract) = ???
}