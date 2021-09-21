package funar

object Intro {

  // Scala 2 enumerations: design mistake

//  sealed trait Liveness // trait - think "interface"
//  case object Alive extends Liveness // case: automatically define constructors, equality, etc.
//  case object Dead extends Liveness

  sealed trait Liveness {
    case object Alive extends Liveness
    case object Dead extends Liveness
  }

}
