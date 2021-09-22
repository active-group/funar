package funar

import scala.language.implicitConversions

// +, *, above, beside, overlay
// Set/Type A
// op(x: A, y: A): A
// associativity
// x + (y + z) = (x + y) + z
// op(x, op(y, z)) == op(op(x, y), z)
// semigroup: A + op + associativity
trait Semigroup[A] {
  // associativity must hold
  def op(x: A, y: A): A
}

class AdditiveSemigroup() extends Semigroup[Int] {
  def op(x: Int, y: Int): Int = x + y
}

// semigroup + identity
trait Monoid[A] extends Semigroup[A] {
  def identity: A

  def ops(xs: Seq[A]): A =
    xs.foldLeft(identity)(op)
}


case class MonoidPackage[A](x: A, monoid: Monoid[A]) {
  def op(y: A): A = monoid.op(x, y)
}

object Monoid {
  implicit object AdditiveMonoid extends AdditiveSemigroup with Monoid[Int] {
    def identity = 0
  }

  def listMonoid = new Monoid[List[A]] {
    
  }

  // pattern: type class (cf. Haskell)
  implicit def pack[A](x: A)(implicit monoid: Monoid[A]): MonoidPackage[A] =
    MonoidPackage(x, monoid)

  val foo1 = AdditiveMonoid.op(4, 5) 
  val foo2 = MonoidPackage(4, AdditiveMonoid).op(5)
  val foo3 = pack(4)(AdditiveMonoid).op(5)
  val foo4 = pack(4).op(5)
  val foo5 = 4.op(5)
  val foo6 = 4 op 5
}