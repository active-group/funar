package funar

/*
1. ask for a simple example
Zero-Coupon Bond
"receive 100EUR on Dec 24 2021"
2. smash that into pieces / separate ideas

- amount
- currency
- later

3. ask for another example
   Either it's already possible; if not, repeat.

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
case class Later(date: Date, contract: Contract) extends Contract
case class And(contract1: Contract, contract2: Contract) extends Contract

object Contract {
  val oneeur = One(EUR)
  val eur100 = Multiple(100, One(EUR))
  val zcb1 = Later(Date("2021-12-24"), Multiple(100, One(EUR)))

  def zeroCouponBond(amount: Double, currency: Currency, date: Date): Contract =
    Later(date, Multiple(amount, One(currency)))

}