package funar

/*
1. ask for a simple example
Zero-Coupon Bond
"receive 100EUR on Dec 24 2021"

*/
sealed trait Currency
case object CHF extends Currency
case object EUR extends Currency
case object GBP extends Currency

case class ZeroCouponBond(amount: Double, currency: Currency, date: Date)