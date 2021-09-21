package funar

import scala.annotation.tailrec
import scala.language.implicitConversions

object Intro {

  def apply(x: Int): Int = x + 1

  // Scala 2 enumerations: design mistake

  sealed trait Liveness // trait - think "interface"
//  case object Alive extends Liveness // case: automatically define constructors, equality, etc.
//  case object Dead extends Liveness

  // companion object
  object Liveness {
    case object Alive extends Liveness
    case object Dead extends Liveness
  }

  type Weight = Int

  // sealed: all subtypes in this file
  sealed trait Animal {
    def runOver: Animal // returns Animal

    def runOver2: Animal =
      this match {
        case Dillo(liveness, weight) => Dillo(Liveness.Dead, weight)
        case Snake(length, thickness) => Snake(length, 0)
      }
  }

  case class Dillo(liveness: Liveness, weight: Weight) extends Animal {
    override def runOver: Dillo = // no arguments: empty parens
      this.copy(liveness = Liveness.Dead)
  }

  // automatically defines a companion object with apply method: constructor
  /*
  object Dillo {
    def apply(liveness: Liveness, weight: Weight) = new Dillo(..., ...)
  }
  */

  // values
  val dillo1 = Dillo(Liveness.Alive, 10)
  val dillo2 = Dillo(Liveness.Dead, 11)
  
  // function
  def runOverDillo(dillo: Dillo): Dillo =
    // Dillo(Liveness.Dead, dillo.weight)
    // dillo.copy(liveness = Liveness.Dead)
    dillo match {
      case Dillo(Liveness.Dead, _) => dillo
      case Dillo(_, w) => Dillo(Liveness.Dead, w)
    }

  case class Snake(length: Int, thickness: Int) extends Animal {
    override def runOver: Snake =
      this.copy(thickness = 0)
  }

  val snake1 = Snake(200, 5)
  val snake2 = Snake(300, 10)

  def runOverSnake(snake: Snake): Snake =
    snake.copy(thickness = 0)


  def runOverAnimal(animal: Animal): Animal =
    animal match {
      case Snake(length, thickness) => runOverSnake(animal.asInstanceOf[Snake]) // Snake(length, 0)
      case Dillo(_, weight) => Dillo(Liveness.Dead, weight)
    }

  val highway: List[Animal] = dillo1 :: (dillo2 :: (snake1 :: (snake2 :: Nil)))

  // Lisp: first = car / rest = cdr

  /*
  def runOverAnimals(animals: List[Animal]): List[Animal] =
    animals match {
      case Nil => Nil
      case first :: rest => first.runOver :: runOverAnimals(rest)
    }
  */

  def runOverAnimals(animals: List[Animal]): List[Animal] = {
    @tailrec  
    def loop(animals: List[Animal], acc: List[Animal]): List[Animal] =
      animals match {
        case Nil => acc.reverse
        case first :: rest => loop(rest, runOverAnimal(first) :: acc)
      }
    loop(animals, Nil)
  }

  val list4 = List(5, 7, 12, 13)

  // (list-fold 0 + (list 5 7 12 13) 
  val foo1 = list4.foldRight(0)( (a, b) => a + b)
  val foo2 = list4.foldRight(0) { (a, b) => a + b }
  val foo3 = list4.foldRight(0)(_ + _)
  val foo4 = list4.map { x => x+ 1 }

  def plus(x: Int, y: Int): Int = x + y

  def plus1(x: Int): Int => Int = { y => x + y}

  def plus2(x: Int)(y: Int) = x + y

  def foldRight[A, B](forNil: B, forCons: (A, B) => B, list: List[A]): B =
    list match {
      case Nil => forNil
      case first::rest =>
        forCons(first, foldRight(forNil, forCons, rest))
    }

  // type inference fails:
  // val foo5 = foldRight(0, _ + _, list4)

  // unary functions are equivalent to n-ary functions:
  // Haskell Curry 
  // Moses Schönfinkel

  def curry[A, B, C](f: (A, B) => C): A => (B => C) =
    { a => { b => f(a, b) }}

  def uncurry[A, B, C](f: A => (B => C)): (A, B) => C =
    { (a, b) => f(a)(b) }

  // tuple:
  val map1 = Map(("Mike", "Sperber"), "Lars" -> "Winterhalder")

  /*
  sealed trait Option[A]

  case object None extends Option[A]
  case object Some[A](value: A) extends Option[A]
  */

  case class IntWithRange(x: Int) {
    def rangeUpTo(y: Int) = x.to(y)
  }

  implicit def toIntWithRange(x: Int): IntWithRange = IntWithRange(x)

  implicit def fromIntWithRange(intgr: IntWithRange): Int = intgr.x

  val bar = 15.rangeUpTo(17)

}
