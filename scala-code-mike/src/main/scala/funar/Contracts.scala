package funar

/*
1. ask for a simple example
Zero-Coupon Bond
"receive 100EUR on Dec 24 2021"
2. smash that into pieces / separate ideas

- amount
- currency
- later

*/
sealed trait Currency
case object CHF extends Currency
case object EUR extends Currency
case object GBP extends Currency

// Date("2021-12-24")
case class Date(desc: String) extends Ordered[Date] {
  def compare(that: Date): Int =
    this.desc.compare(that.desc)
}

// does not scale
/*
sealed trait Contract
case class ZeroCouponBond(amount: Double, currency: Currency, date: Date) extends Contract
case class CurrencySwap(/* ... */) extends Contract
*/

sealed trait Contract
// "one EUR now"
case class One(currency: Currency) extends Contract
case class Multiple(amount: Double, contract: Contract) extends Contract


object Contract {
  val oneeur = One(EUR)
}