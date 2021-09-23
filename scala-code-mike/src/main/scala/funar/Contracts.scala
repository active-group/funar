package funar

/*
1. ask for a simple example
Zero-Coupon Bond
"receive 100EUR on Dec 24 2021"
2. smash that into pieces / separate ideas

- amount
- currency
- later

3. find combinators

4. ask for another example
   Either it's already possible; if not, repeat.


   currency swap:
     on Dec 24 2021: receive 100EUR, pay 100CHF

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
case object Zero extends Contract
case class One(currency: Currency) extends Contract
case class Multiple(amount: Double, contract: Contract) extends Contract
case class Later(date: Date, contract: Contract) extends Contract
// asssociative in meaning: yay, semigroup
case class And(contract1: Contract, contract2: Contract) extends Contract
/// case class PayCurrency(currency: Currency) extends Contract
// equivalently - inverts payment streams / swaps rights and obligations
case class Pay(contract: Contract) extends Contract


object Contract {
  val oneeur = One(EUR)
  val eur100 = Multiple(100, One(EUR))
  val zcb1 = Later(Date("2021-12-24"), Multiple(100, One(EUR)))

  def zeroCouponBond(amount: Double, currency: Currency, date: Date): Contract =
    Later(date, Multiple(amount, One(currency)))

  val zcb2 = zeroCouponBond(100, CHF, Date("2021-12-24"))
  val swap = And(zcb1, Pay(zcb2))


  sealed trait Direction {
    def flip: Direction
  }
  case object Long extends Direction {
    def flip = Short
  }
  case object Short extends Direction {
    def flip = Long
  }

  case class Payment(date: Date, direction: Direction, amount: Double, currency: Currency) {
    def multiply(factor: Double): Payment = this.copy(amount = amount * factor)
    def flip: Payment = this.copy(direction = direction.flip)
  }

  // returns payments & residual contract
  def payments(contract: Contract, today: Date): (Seq[Payment], Contract) =
    contract match {
      case Zero => (Seq.empty, Zero)
      case One(currency) => (Seq(Payment(today, Long, 1, currency)), Zero)
      case Multiple(amount, contract) => {
        val (ps, res) = payments(contract, today)
        (ps.map(_.multiply(amount)), Multiple(amount, res)) 
      }

      case Later(date, contract) => 
        if (today >= date)
          payments(contract, today)
        else
          (Seq.empty, Later(date, contract))
      case And(contract1, contract2) => {
        val (ps1, res1) = payments(contract1, today)
        val (ps2, res2) = payments(contract2, today)
        (ps1++ps2, And(res1, res2))
      }
      case Pay(contract) => {
        val (ps, res) = payments(contract, today)
        (ps.map(_.flip), Pay(res))
      }
        
    }

}