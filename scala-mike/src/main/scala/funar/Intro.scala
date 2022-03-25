package funar

// note: there are two Scala versions right now
// Scala 2 and Scala 3

val s: String = "Mike"

// A pet is one of the following:
// - a dog - OR -
// - a cat - OR -
// - a snake
// mixed data
// trait == interface
// enumeration
/*
sealed trait Pet
/*
case class Dog() extends Pet
case class Cat() extends Pet 
case class Snake() extends Pet 
*/
case object Dog extends Pet 
case object Cat extends Pet  
case object Snake extends Pet 
*/

enum Pet {
  case Dog
  case Cat
  case Snake
}

val p0 = Pet.Snake

import Pet._

val p1: Pet = Dog
val p2: Pet = Cat

// Is a pet cute?
// def defines a function
/*
def isCute(pet: Pet): Boolean =
  // template / "Schablone"
  pet match { // pattern matching
    case Cat => ???
    case Dog => ???
    case Snake => ???
  }
*/ 
def isCute(pet: Pet): Boolean =
  // template / "Schablone"
  pet match { // pattern matching
    case Cat => true
    case Dog => true
    case Snake => false
  }

// Animals on the Texas Highway

// An armadillo has the following properties:
// - dead - OR - alive
// - weight

enum Liveness {
  case Dead
  case Alive
}

type Weight = Double

case class Dillo(liveness: Liveness, weight: Weight)

val dillo1 = Dillo(Liveness.Alive, 10)
val dillo2 = Dillo(Liveness.Dead, 8)

// run over an armadillo
def runOverDillo(dillo: Dillo): Dillo =
  Dillo(Dead, dillo.weight)


def silly(x: Int, y: Int) = {
  val sum = x + y
  sum * 2
}

